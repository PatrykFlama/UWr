#include <avr/interrupt.h>
#include <avr/io.h>
#include <stdint.h>
#include <stdio.h>

#include "FreeRTOS.h"
#include "semphr.h"
#include "task.h"
#include "uart.h"

/******************************************************************************
 * Private macro definitions.
 ******************************************************************************/

#define mainADC_TASK_PRIORITY 2
#define mainBLINK_TASK_PRIORITY 1

#define mainADC_STACK (configMINIMAL_STACK_SIZE + 64)
#define mainBLINK_STACK (configMINIMAL_STACK_SIZE)

typedef struct {
    uint8_t channel;
    TickType_t delay_ms;
} AdcTaskParams_t;

/******************************************************************************
 * Function prototypes.
 ******************************************************************************/

static void vBlinkLed(void* pvParameters);
static void vAdcTask(void* pvParameters);
static void adc_init(void);
static uint16_t readADC(uint8_t mux);
void vApplicationIdleHook(void);

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

int main(void) {
    xTaskHandle blink_handle;
    xTaskHandle adc0_handle;
    xTaskHandle adc1_handle;
    xTaskHandle adc2_handle;

    static AdcTaskParams_t adc0_params = {0, 700};
    static AdcTaskParams_t adc1_params = {0b1110, 1100};
    static AdcTaskParams_t adc2_params = {0b1111, 1500};

    uart_init();
    stdin = stdout = stderr = &uart_file;

    adc_init();

    xTaskCreate(
        vBlinkLed,
        "blink",
        mainBLINK_STACK,
        NULL,
        mainBLINK_TASK_PRIORITY,
        &blink_handle);

    xTaskCreate(
        vAdcTask,
        "adc0",
        mainADC_STACK,
        &adc0_params,
        mainADC_TASK_PRIORITY,
        &adc0_handle);

    xTaskCreate(
        vAdcTask,
        "adc1",
        mainADC_STACK,
        &adc1_params,
        mainADC_TASK_PRIORITY,
        &adc1_handle);

    xTaskCreate(
        vAdcTask,
        "adc2",
        mainADC_STACK,
        &adc2_params,
        mainADC_TASK_PRIORITY,
        &adc2_handle);

    vTaskStartScheduler();

    return 0;
}

void vApplicationIdleHook(void) {
}

/******************************************************************************
 * Function definitions.
 ******************************************************************************/

static void vBlinkLed(void* pvParameters) {
    (void)pvParameters;
    DDRB |= _BV(PB5);

    for (;;) {
        PORTB ^= _BV(PB5);
        vTaskDelay(500 / portTICK_PERIOD_MS);
    }
}


static SemaphoreHandle_t adc_mutex;
static SemaphoreHandle_t adc_done;
static SemaphoreHandle_t uart_mutex;
static volatile uint16_t adc_value;

static void vAdcTask(void* pvParameters) {
    AdcTaskParams_t* params = (AdcTaskParams_t*)pvParameters;
    uint8_t channel = params->channel;
    TickType_t delay_ms = params->delay_ms;

    for (;;) {
        uint16_t value = readADC(channel);
        
        xSemaphoreTake(uart_mutex, portMAX_DELAY);
        printf("ADC%u = %u\r\n", channel, value);
        xSemaphoreGive(uart_mutex);
        
        vTaskDelay(delay_ms / portTICK_PERIOD_MS);
    }
}

static void adc_init() {
    // Create synchronization primitives
    adc_mutex = xSemaphoreCreateBinary();
    adc_done = xSemaphoreCreateBinary();
    uart_mutex = xSemaphoreCreateBinary();

    // release mutexes
    xSemaphoreGive(adc_mutex);
    xSemaphoreGive(uart_mutex);

    // ensure adc_done is empty
    xSemaphoreTake(adc_done, 1);

    // AVcc reference
    ADMUX = _BV(REFS0);

    // disable digital inputs
    DIDR0 = _BV(ADC0D) | _BV(ADC1D) | _BV(ADC2D);

    // prescaler 128
    ADCSRA = _BV(ADPS0) | _BV(ADPS1) | _BV(ADPS2);

    // enable ADC and interrupt
    ADCSRA |= _BV(ADEN) | _BV(ADIE);
}

static uint16_t readADC(uint8_t mux) {
    uint16_t result;

    // serialize access to ADC hardware
    xSemaphoreTake(adc_mutex, portMAX_DELAY);

    // select channel (keep reference bits)
    ADMUX = (ADMUX & 0xF0) | (mux & 0x0F);

    // start conversion
    ADCSRA |= _BV(ADSC);

    // wait for conversion complete signaled by ISR
    xSemaphoreTake(adc_done, portMAX_DELAY);

    result = adc_value;

    xSemaphoreGive(adc_mutex);
    return result;
}

ISR(ADC_vect) {
    BaseType_t xHigherPriorityTaskWoken = pdFALSE;

    adc_value = ADC;     // latch result
    ADCSRA |= _BV(ADIF); // clear interrupt flag

    xSemaphoreGiveFromISR(adc_done, &xHigherPriorityTaskWoken);
}
