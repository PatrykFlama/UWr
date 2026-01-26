#include <avr/interrupt.h>
#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <util/delay.h>

#include "../../customlib/uart.c"
#include "../AVR221/IAR/pid.h"

// ============================================================================
// Configuration
// ============================================================================

// Phase and Frequency Correct PWM on Timer1
// Frequency: ~1 kHz (bidirectional counter)
// F_PWM = F_CPU / (2 * prescaler * TOP)
// 1000 = 16000000 / (2 * 8 * TOP)
// TOP = 1000
#define PWM_PRESCALER 8
#define PWM_TOP 1000

// ADC reference
#define VREF_MV 5000UL

// PID controller scaling
#define SCALING_FACTOR  128

// Initial PID parameters for speed control
#define PID_P   100
#define PID_I   30
#define PID_D   40

// ============================================================================
// Global Variables
// ============================================================================

// ADC measurement results (captured at TOP and BOTTOM)
static volatile uint16_t adc_at_top = 0;     // motor EMF at TOP (transistor closed)
static volatile uint16_t adc_at_bottom = 0;  // motor voltage at BOTTOM (transistor open)
static volatile uint8_t adc_ready = 0;       // flag when measurements complete

// Current motor speed (mV, representing EMF)
static volatile uint16_t current_speed_mv = 0;

// Speed setpoint from potentiometer (mV)
static uint16_t target_speed_mv = 2500;  // default: half speed

// ============================================================================
// PID Data Structure
// ============================================================================

pidData_t pid_speed;
uint16_t pwm_duty = 0;

// ============================================================================
// ADC Functions
// ============================================================================

static void adc_init() {
    // AVcc reference, ADC0 (potentiometer) and ADC1 (motor voltage)
    ADMUX = _BV(REFS0);
    DIDR0 = _BV(ADC0D) | _BV(ADC1D);  // disable digital input
    // ADC prescaler 128 (125 kHz at 16 MHz)
    ADCSRA = _BV(ADEN) | _BV(ADPS2) | _BV(ADPS1) | _BV(ADPS0);
}

static uint16_t adc0_read() {
    ADMUX = _BV(REFS0);  // ADC0 (PC0) - potentiometer
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

static uint16_t adc1_read() {
    ADMUX = _BV(REFS0) | _BV(MUX0);  // ADC1 (PC1) - motor voltage
    ADCSRA |= _BV(ADSC);
    while (ADCSRA & _BV(ADSC));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// ============================================================================
// PWM Functions
// ============================================================================

static void pwm_init() {
    // Configure PB1 (OC1A) as output
    DDRB |= _BV(PB1);

    // Phase and Frequency Correct PWM, TOP=ICR1, non-inverting
    // COM1A = 10 (non-inverting)
    // WGM1 = 1000 (Phase and Frequency Correct PWM, TOP=ICR1)
    // CS1 = 010 (prescaler 8)
    ICR1 = PWM_TOP;
    TCCR1A = _BV(COM1A1);
    TCCR1B = _BV(WGM13) | _BV(CS11);

    // Initial PWM duty: 0 (motor off)
    OCR1A = 0;

    // Interrupts: ICIE1 (at TOP) and TOIE1 (at BOTTOM)
    TIMSK1 = _BV(ICIE1) | _BV(TOIE1);
}

static void pwm_set(uint16_t duty) {
    if (duty > PWM_TOP) duty = PWM_TOP;
    OCR1A = duty;
    pwm_duty = duty;
}

// ============================================================================
// Interrupt Handlers
// ============================================================================

// Timer1 Input Capture (TOP) - Transistor closed
// Measure motor EMF (speed indication)
ISR(TIMER1_CAPT_vect) {
    adc_at_top = adc1_read();
}

// Timer1 Overflow (BOTTOM) - Transistor open
// Both measurements are ready
ISR(TIMER1_OVF_vect) {
    adc_at_bottom = adc1_read();
    adc_ready = 1;
}

// ============================================================================
// Speed Control Update
// ============================================================================

void update_speed_control() {
    if (!adc_ready) return;

    cli();
    uint16_t top_adc = adc_at_top;
    adc_ready = 0;
    sei();

    // Convert ADC reading to voltage (mV)
    uint32_t top_mv = ((uint32_t)top_adc * VREF_MV) / 1023UL;

    // Motor EMF: when transistor closed, we measure the motor back-EMF
    // EMF rises with speed, napięcie maleję with speed (for constant V supply)
    // actual_motor_emf = Vsupply - V_measured
    uint32_t motor_emf_mv = VREF_MV - top_mv;

    // Update current speed for PID
    current_speed_mv = (uint16_t)(motor_emf_mv & 0xFFFF);

    // Clamp speed to 0-5000 mV range
    if (current_speed_mv > VREF_MV) current_speed_mv = VREF_MV;

    // PID control: adjust PWM based on speed error
    // Speed setpoint is in mV (0-5000)
    int16_t pid_output = pid_Controller(target_speed_mv, current_speed_mv, &pid_speed);

    // Clamp output to PWM range (0-PWM_TOP)
    if (pid_output > PWM_TOP) pid_output = PWM_TOP;
    if (pid_output < 0) pid_output = 0;

    pwm_set(pid_output);
}

// ============================================================================
// Serial Command Handler
// ============================================================================

void command_handler() {
    printf("\n\rCommands:\n\r");
    printf("  S<value> - Set target speed (0-5000 mV)\n\r");
    printf("  P<value> - Set P factor (e.g., P100)\n\r");
    printf("  I<value> - Set I factor (e.g., I30)\n\r");
    printf("  D<value> - Set D factor (e.g., D40)\n\r");
    printf("  R        - Reset PID integrator\n\r");

    char line[32];
    uart_readline(line, sizeof(line));

    if (strlen(line) == 0) return;

    char cmd = line[0];
    switch (cmd) {
        case 'S':
        case 's': {
            int val = atoi(&line[1]);
            if (val > 5000) val = 5000;
            if (val < 0) val = 0;
            target_speed_mv = val;
            printf("Target speed set to %d mV\n\r", val);
            break;
        }
        case 'P':
        case 'p': {
            int val = atoi(&line[1]);
            pid_speed.P_Factor = val;
            pid_speed.maxError = INT16_MAX / (val + 1);
            printf("P factor set to %d\n\r", val);
            break;
        }
        case 'I':
        case 'i': {
            int val = atoi(&line[1]);
            pid_speed.I_Factor = val;
            pid_speed.maxSumError = (INT32_MAX / 2) / (val + 1);
            printf("I factor set to %d\n\r", val);
            break;
        }
        case 'D':
        case 'd': {
            int val = atoi(&line[1]);
            pid_speed.D_Factor = val;
            printf("D factor set to %d\n\r", val);
            break;
        }
        case 'R':
        case 'r': {
            pid_Reset_Integrator(&pid_speed);
            printf("PID integrator reset\n\r");
            break;
        }
        default:
            printf("Unknown command. Send any character for help.\n\r");
    }
}

// ============================================================================
// Main
// ============================================================================

int main() {
    uart_init();
    adc_init();
    pwm_init();

    // Initialize PID controller for speed control
    pid_Init(PID_P, PID_I, PID_D, &pid_speed);

    sei();

    printf("\n\r=== DC Motor Speed Controller with PID ===\n\r");
    printf("Using Phase and Frequency Correct PWM\n\r");
    printf("Send any character for help\n\r");

    uint16_t loop_count = 0;

    while (1) {
        // Update speed from PID controller
        update_speed_control();

        // Every 100ms, read potentiometer and display status
        if (loop_count % 10 == 0) {
            uint16_t pot_value = adc0_read();
            // Potentiometer voltage -> target speed mapping
            uint32_t pot_speed = ((uint32_t)pot_value * VREF_MV) / 1023UL;

            // Update target speed from potentiometer
            target_speed_mv = (uint16_t)pot_speed;

            printf("POT: %4u mV | Speed: %4u mV | Target: %4u mV | PWM: %4u/%u\r",
                   (uint16_t)pot_speed, current_speed_mv, target_speed_mv, pwm_duty, PWM_TOP);
            fflush(stdout);
        }

        if (UCSR0A & _BV(RXC0)) {
            command_handler();
        }

        loop_count++;
        _delay_ms(10);  // 100 Hz update rate
    }
}
