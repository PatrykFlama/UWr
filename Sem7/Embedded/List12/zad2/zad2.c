#include <avr/interrupt.h>
#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <util/delay.h>

#include "../../customlib/uart.c"
#include "../AVR221/IAR/pid.h"

// Phase and Frequency Correct PWM na Timer1
// Częstotliwość PWM: ~1 kHz (dwukierunkowy licznik)
// F_PWM = F_CPU / (2 * prescaler * TOP)
// 1000 = 16000000 / (2 * 8 * TOP)
// TOP = 1000
#define PWM_PRESCALER 8
#define PWM_TOP 1000

// AVcc = 5000 mV
#define VREF_MV 5000UL

// Voltage thresholds for speed estimation
// Zatrzymany silnik: U_motor ≈ 5V (V_measured ≈ 0V)
// Silnik z prędkością: U_motor < 5V (V_measured > 0V)
#define MAX_EMF_MV 5000UL
#define MIN_SPEED_MV 500UL   // ~10% prędkości = 500 mV
#define MAX_SPEED_MV 4500UL  // ~90% prędkości = 4500 mV

// wyniki ADC
static volatile uint16_t adc_at_top = 0;     // pomiar przy TOP (tranzystor zamknięty)
static volatile uint16_t adc_at_bottom = 0;  // pomiar przy BOTTOM (tranzystor otwarty)
static volatile uint8_t adc_ready = 0;       // flaga gotowych pomiarów

// inicjalizacja ADC na kanałach ADC0 i ADC1
static void adc_init() {
    ADMUX = _BV(REFS0);                                 // AVcc as ref, ADC0 (PC0)
    DIDR0 = _BV(ADC0D) | _BV(ADC1D);                    // disable digital inputs
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);  // prescaler 128 (125 kHz)
}

// odczyt z ADC0 (potencjometr - docelowa prędkość)
static uint16_t adc0_read() {
    ADMUX = _BV(REFS0); // ADC0 (PC0)
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// inicjalizacja Timer1 jako Phase and Frequency Correct PWM
static void pwm_init() {
    // ustaw PB1 (OC1A) jako wyjście
    DDRB |= _BV(PB1);

    // Phase and Frequency Correct PWM, TOP=ICR1, non-inverting mode
    // COM1A = 10   -- non-inverting
    // WGM1  = 1000 -- Phase and Frequency Correct PWM, TOP=ICR1
    // CS1   = 010  -- preskaler 8
    ICR1 = PWM_TOP;
    TCCR1A = _BV(COM1A1);
    TCCR1B = _BV(WGM13) | _BV(CS11);

    OCR1A = 0;  // początkowe wypełnienie 0

    // przerwania: CAPT (TOP) i OVF (BOTTOM)
    TIMSK1 = _BV(ICIE1) | _BV(TOIE1);
}

// odczyt z ADC1 (pomiary silnika)
static uint16_t adc1_read() {
    ADMUX = _BV(REFS0) | _BV(MUX0); // ADC1 (PC1)
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// przerwanie Input Capture (TOP) - tranzystor zamknięty
ISR(TIMER1_CAPT_vect) {
    adc_at_top = adc1_read();
}

// przerwanie Overflow (BOTTOM) - tranzystor otwarty
ISR(TIMER1_OVF_vect) {
    adc_at_bottom = adc1_read();
    adc_ready = 1;
}

// Convert motor EMF voltage to speed estimate (0-1023 scale for PID)
// Motor EMF = 5V - V_measured
// When motor is stopped: EMF ≈ 5V → speed = 0
// When motor runs fast: EMF ≈ 0V → speed = 1023 (max)
// Scale: speed = 1023 - (EMF / 5V) * 1023
static int16_t motor_emf_to_speed(uint32_t emf_mv) {
    if (emf_mv > MAX_EMF_MV) emf_mv = MAX_EMF_MV;
    
    // Speed proportional to voltage drop: higher EMF = lower speed
    int16_t speed = (int16_t)(1023 - ((emf_mv * 1023UL) / MAX_EMF_MV));
    
    if (speed < 0) speed = 0;
    if (speed > 1023) speed = 1023;
    
    return speed;
}

void print_status(uint16_t duty, uint32_t emf_mv, uint32_t current_mv, int16_t current_speed, int16_t target_speed) {
    printf("PWM:%3u%%|EMF:%4lu mV|Current:%3lu mV|Speed:%4d|Target:%4d\r",
           (duty * 100) / PWM_TOP,
           emf_mv,
           current_mv,
           current_speed,
           target_speed);
    fflush(stdout);
}

void print_help() {
    printf("\n\rSpeed Control with PID\n\r");
    printf("Commands:\n\r");
    printf("  S<value> - Set target speed manually (0-1023), e.g., S500\n\r");
    printf("  P<value> - Set P coefficient (e.g., P50)\n\r");
    printf("  I<value> - Set I coefficient (e.g., I5)\n\r");
    printf("  D<value> - Set D coefficient (e.g., D100)\n\r");
    printf("  ?        - Show this help\n\r");
    printf("  Use potentiometer (ADC0) for target speed or set manually\n\r");
}

int main() {
    uart_init();
    adc_init();
    pwm_init();

    // Initialize PID controller
    // Starting tuning: Kp=50, Ki=5, Kd=100
    pidData_t pid_data;
    pid_Init(50, 5, 100, &pid_data);

    int16_t target_speed_manual = -1;  // -1 means use potentiometer
    uint16_t loop_count = 0;

    sei();

    printf("Motor Speed Controller with PID initialized\n\r");
    printf("Type '?' for help\n\r");

    while (1) {
        // Determine target speed: manual setting or potentiometer
        int16_t target_speed;
        if (target_speed_manual >= 0) {
            target_speed = target_speed_manual;
        } else {
            // Read potentiometer (ADC0) for target speed
            uint16_t pot_value = adc0_read();
            target_speed = (int16_t)((pot_value * 1023UL) / 1023UL);  // 0-1023 range
        }

        while (!adc_ready);

        cli();
        uint16_t top_adc = adc_at_top;
        uint16_t bottom_adc = adc_at_bottom;
        adc_ready = 0;
        sei();

        // Convert ADC readings to voltages
        uint32_t top_mv = ((uint32_t)top_adc * VREF_MV) / 1023UL;
        uint32_t bottom_mv = ((uint32_t)bottom_adc * VREF_MV) / 1023UL;

        // Calculate motor EMF and current
        // EMF = 5V - V_measured (at TOP when transistor is closed)
        uint32_t motor_emf_mv = VREF_MV - top_mv;
        uint32_t motor_current_mv = bottom_mv;

        // Estimate current speed from motor EMF
        int16_t current_speed = motor_emf_to_speed(motor_emf_mv);

        // Calculate PID output (-1023 to 1023)
        int16_t pid_output = pid_Controller(target_speed, current_speed, &pid_data);

        // Map PID output to PWM duty cycle (0 to PWM_TOP)
        uint16_t duty = 0;
        if (pid_output > 0) {
            // Scale positive PID output to duty cycle
            duty = (uint16_t)(((int32_t)pid_output * (int32_t)PWM_TOP) / 1024);
            if (duty > PWM_TOP) duty = PWM_TOP;
        }

        OCR1A = duty;

        // Display status every ~500ms
        if (loop_count % 5 == 0) {
            print_status(duty, motor_emf_mv, motor_current_mv, current_speed, target_speed);
        }

        // Check for UART commands
        if (UCSR0A & _BV(RXC0)) {
            uint8_t ch = UDR0;
            
            if (ch == '?') {
                print_help();
            }
            else if (ch == 'S' || ch == 's') {
                printf("\n\rEnter target speed (0-1023): ");
                fflush(stdout);
                char line[10];
                uart_readline(line, sizeof(line));
                int val = atoi(line);
                if (val >= 0 && val <= 1023) {
                    target_speed_manual = val;
                    pid_Reset_Integrator(&pid_data);
                    printf("Target speed set to %d\n\r", val);
                } else {
                    printf("Invalid value (0-1023)\n\r");
                    target_speed_manual = -1;  // back to potentiometer
                }
            }
            else if (ch == 'P' || ch == 'p') {
                printf("\n\rEnter P coefficient: ");
                fflush(stdout);
                char line[10];
                uart_readline(line, sizeof(line));
                int val = atoi(line);
                pid_data.P_Factor = val;
                printf("P set to %d\n\r", val);
            }
            else if (ch == 'I' || ch == 'i') {
                printf("\n\rEnter I coefficient: ");
                fflush(stdout);
                char line[10];
                uart_readline(line, sizeof(line));
                int val = atoi(line);
                pid_data.I_Factor = val;
                pid_Reset_Integrator(&pid_data);
                printf("I set to %d\n\r", val);
            }
            else if (ch == 'D' || ch == 'd') {
                printf("\n\rEnter D coefficient: ");
                fflush(stdout);
                char line[10];
                uart_readline(line, sizeof(line));
                int val = atoi(line);
                pid_data.D_Factor = val;
                printf("D set to %d\n\r", val);
            }
        }

        loop_count++;
        _delay_ms(100);
    }
}
