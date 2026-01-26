#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#include "../../customlib/uart.c"
#include "../AVR221/IAR/pid.h"

// ADC channel selection
#define ADC_CHANNEL 2

// MOSFET gate
#define HEATER_DDR  DDRD
#define HEATER_PORT PORTD
#define HEATER_PIN  _BV(PD5)

// MCP9700 constants
// Vout = Tc * Ta + V0
// Ta = ambient; Tc = temperature coefficient
#define TC 0.01f    // 10 mV/C
#define V0 0.5f     // 500 mV at 0C

// ADC 1.1V reference
void adc_init() {
    // 1.1V internal reference and ADC channel
    ADMUX = _BV(REFS1) | _BV(REFS0) | ADC_CHANNEL;  // 1.1V internal ref + channel selection
    DIDR0 = _BV(ADC_CHANNEL);  // disable digital input on selected ADC channel
    // ADC prescaler 128 for ~125 kHz at 16 MHz
    ADCSRA = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2) | _BV(ADEN);
}

uint16_t adc_read() {
    ADCSRA |= _BV(ADSC);
    while (!(ADCSRA & _BV(ADIF)));
    ADCSRA |= _BV(ADIF);
    return ADC;
}

// ADC value to temperature in 0.1C
int16_t adc_to_temp(uint16_t adc_val) {
    return (int16_t)(((adc_val * 1.1f / 1024.0f) - V0) / TC * 10.0f);
}

void heater_init() {
    // confing output pin
    HEATER_DDR |= HEATER_PIN;
    
    // Timer0 Fast PWM (8-bit), prescaler 64
    // WGM0[2:0] = 011 (Fast PWM)
    // COM0B[1:0] = 10 (non-inverting on OC0B)
    // CS0[2:0] = 011 (prescaler 64)
    // Frequency: 16MHz / (64 * 256) ≈ 977 Hz
    TCCR0A = _BV(COM0B1) | _BV(WGM01) | _BV(WGM00);
    TCCR0B = _BV(CS01) | _BV(CS00);
    
    OCR0B = 0; // off
}

void heater_set_pwm(int16_t pwm_value) {
    if (pwm_value < 0) pwm_value = 0;
    if (pwm_value > 255) pwm_value = 255;
    OCR0B = pwm_value;  // set pwm
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

// handle commands
void handle_commands(int16_t *target_temp, pidData_t *pid) {
    if (!(UCSR0A & _BV(RXC0))) return;
    
    uint8_t ch = UDR0;
    
    if (ch == '?') {
        print_help();
    } else if (ch == 'T' || ch == 't') {
        printf("\n\rEnter target temp (C): ");
        fflush(stdout);
        char line[10];
        uart_readline(line, sizeof(line));
        int val = atoi(line);
        *target_temp = val * 10;  // convert to 0.1C units
        pid_Reset_Integrator(pid);  // reset integrator on setpoint change
        printf("Target set to %d.%dC\n\r", val, 0);
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
        pid_Reset_Integrator(pid);  // reset integrator on I change
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

    // init PID controller
    // P_Factor, I_Factor, D_Factor (these are multiplied by SCALING_FACTOR=128)
    pidData_t pid_data;
    pid_Init(80, 10, 100, &pid_data);
    
    int16_t target_temp = 300;  // 30.0C in 0.1C units
    uint16_t loop_count = 0;

    printf("Temperature controller with PID initialized\n\r");
    printf("Type '?' for help\n\r");

    while (1) {
        // read temperature
        uint16_t adc_val = adc_read();
        int16_t current_temp = adc_to_temp(adc_val);

        int16_t pid_output = pid_Controller(target_temp, current_temp, &pid_data);
        
        // PID output to PWM (0-255)
        int16_t pwm_value = pid_output / 2;
        if (pwm_value < 0) pwm_value = 0;
        if (pwm_value > 255) pwm_value = 255;
        
        heater_set_pwm(pwm_value);

        // print status
        if (loop_count % 10 == 0) {
            print_status(current_temp, target_temp, pwm_value);
        }

        // commands
        handle_commands(&target_temp, &pid_data);

        loop_count++;
        _delay_ms(10);
    }
}
