#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "../../customlib/uart.c"
#include "../AVR221/IAR/pid.h"

// MOSFET gate on PC1
#define HEATER_DDR  DDRC
#define HEATER_PORT PORTC
#define HEATER_PIN  (1 << PC1)

// MCP9700 constants
// Vout = Tc * Ta + V0
// Ta = ambient; Tc = temperature coefficient
#define TC 0.01f    // 10 mV/C
#define V0 0.5f     // 500 mV at 0°C

// ADC 1.1V reference
void adc_init() {
    // 1.1V internal reference and ADC0
    ADMUX = _BV(REFS1) | _BV(REFS0); // 1.1V internal ref
    DIDR0 = _BV(ADC0D); // disable digital input on ADC0
    // ADC prescaler 128 for ~125 kHz at 16 MHz
    ADCSRA = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2) | _BV(ADEN);
}

uint16_t adc_read() {
    ADCSRA |= _BV(ADSC);
    while (!(ADCSRA & _BV(ADIF)));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// Convert ADC value to temperature in 0.1°C units
int16_t adc_to_temp(uint16_t adc_val) {
    return (int16_t)(((adc_val * 1.1f / 1024.0f) - V0) / TC * 10.0f);
}

// Heater control with PWM (0-255)
void heater_init() {
    // PB1 (OC1A) for PWM output
    DDRB |= _BV(PB1);
    
    // Timer1 Fast PWM (8-bit), prescaler 8
    // WGM1[3:0] = 0101 (8-bit Fast PWM)
    // COM1A[1:0] = 10 (non-inverting)
    // CS1[2:0] = 010 (prescaler 8)
    TCCR1A = _BV(COM1A1) | _BV(WGM10);
    TCCR1B = _BV(WGM12) | _BV(CS11);
    
    OCR1A = 0; // Initially off
}

void heater_set_pwm(int16_t pwm_value) {
    // Clamp to 0-255
    if (pwm_value < 0) pwm_value = 0;
    if (pwm_value > 255) pwm_value = 255;
    OCR1A = pwm_value;
}

void print_status(int16_t temp, int16_t target, uint8_t pwm) {
    int int_part = temp / 10;
    int frac_part = temp % 10;
    if (frac_part < 0) frac_part = -frac_part;
    
    int target_int = target / 10;
    int target_frac = target % 10;
    
    printf("T:%d.%d C->%d.%d C PWM:%3d%%\r",
           int_part, frac_part,
           target_int, target_frac,
           (pwm * 100) / 255);
    fflush(stdout);
}

void print_help() {
    printf("\n\rCommands:\n\r");
    printf("  T<value> - Set target temp (e.g., T30)\n\r");
    printf("  P<val>   - Set P coefficient (e.g., P80)\n\r");
    printf("  I<val>   - Set I coefficient (e.g., I10)\n\r");
    printf("  D<val>   - Set D coefficient (e.g., D100)\n\r");
    printf("  ?        - Show this help\n\r");
}

// Handle UART commands for temperature and PID tuning
void handle_commands(int16_t *target_temp, pidData_t *pid) {
    if (!(UCSR0A & _BV(RXC0))) return;
    
    uint8_t ch = UDR0;
    
    if (ch == '?') {
        print_help();
    } else if (ch == 'T' || ch == 't') {
        printf("\n\rEnter target temp (°C): ");
        fflush(stdout);
        char line[10];
        uart_readline(line, sizeof(line));
        int val = atoi(line);
        *target_temp = val * 10;  // Convert to 0.1°C units
        pid_Reset_Integrator(pid);  // Reset integrator on setpoint change
        printf("Target set to %d.%d°C\n\r", val, 0);
    } 
    else if (ch == 'P' || ch == 'p') {
        printf("\n\rEnter P coefficient: ");
        fflush(stdout);
        char line[10];
        uart_readline(line, sizeof(line));
        int val = atoi(line);
        pid->P_Factor = val;
        printf("P set to %d\n\r", val);
    }
    else if (ch == 'I' || ch == 'i') {
        printf("\n\rEnter I coefficient: ");
        fflush(stdout);
        char line[10];
        uart_readline(line, sizeof(line));
        int val = atoi(line);
        pid->I_Factor = val;
        pid_Reset_Integrator(pid);  // Reset integrator on I change
        printf("I set to %d\n\r", val);
    }
    else if (ch == 'D' || ch == 'd') {
        printf("\n\rEnter D coefficient: ");
        fflush(stdout);
        char line[10];
        uart_readline(line, sizeof(line));
        int val = atoi(line);
        pid->D_Factor = val;
        printf("D set to %d\n\r", val);
    }
}

int main() {
    uart_init();
    adc_init();
    heater_init();

    // Initialize PID controller
    // P_Factor, I_Factor, D_Factor (these are multiplied by SCALING_FACTOR=128)
    // Initial tuning: Kp=80, Ki=10, Kd=100
    pidData_t pid_data;
    pid_Init(80, 10, 100, &pid_data);
    
    int16_t target_temp = 300;  // 30.0°C in 0.1°C units
    uint16_t loop_count = 0;

    printf("Temperature controller with PID initialized\n\r");
    printf("Type '?' for help\n\r");

    while (1) {
        // Read temperature
        uint16_t adc_val = adc_read();
        int16_t current_temp = adc_to_temp(adc_val);

        // Calculate PID output (-255 to 255 range, then map to 0-255 for PWM)
        int16_t pid_output = pid_Controller(target_temp, current_temp, &pid_data);
        
        // Map PID output to PWM (0-255)
        // PID output range is typically -255 to 255
        int16_t pwm_value = pid_output / 2;  // Scale down to match typical ranges
        if (pwm_value < 0) pwm_value = 0;
        if (pwm_value > 255) pwm_value = 255;
        
        heater_set_pwm(pwm_value);

        // Display status every ~100ms (100 * 10ms)
        if (loop_count % 10 == 0) {
            print_status(current_temp, target_temp, pwm_value);
        }

        // Check for UART commands
        handle_commands(&target_temp, &pid_data);

        loop_count++;
        _delay_ms(10);
    }
}
