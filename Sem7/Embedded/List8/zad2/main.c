#include <avr/io.h>
#include <stdio.h>
#include <stdlib.h>

#include "FreeRTOS.h"
#include "queue.h"
#include "task.h"
#include "uart.h"

/******************************************************************************
 * Private macro definitions.
 ******************************************************************************/

#define mainBLINK_TASK_PRIORITY 2
#define mainSERIAL_TASK_PRIORITY 1

#define mainQUEUE_LENGTH 3
#define mainQUEUE_ITEM_SIZE sizeof(uint16_t)

/******************************************************************************
 * Private function prototypes.
 ******************************************************************************/

static void vBlinkTask(void* pvParameters);
static void vSerialTask(void* pvParameters);
void vApplicationIdleHook(void);

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

int main(void) {
    xTaskHandle blink_handle;
    xTaskHandle serial_handle;

    xQueueHandle queue = xQueueCreate(mainQUEUE_LENGTH, mainQUEUE_ITEM_SIZE);

    xTaskCreate(
        vBlinkTask,
        "blink",
        configMINIMAL_STACK_SIZE,
        (void*)queue,
        mainBLINK_TASK_PRIORITY,
        &blink_handle);

    xTaskCreate(
        vSerialTask,
        "serial",
        configMINIMAL_STACK_SIZE + 32,
        (void*)queue,
        mainSERIAL_TASK_PRIORITY,
        &serial_handle);

    vTaskStartScheduler();

    return 0;
}

void vApplicationIdleHook(void) {
}

/******************************************************************************
 * Private function definitions.
 ******************************************************************************/

static void vBlinkTask(void* pvParameters) {
    xQueueHandle queue = (xQueueHandle)pvParameters;
    uint16_t duration_ms;

    DDRB |= _BV(PB5);

    for (;;) {
        if (xQueueReceive(queue, &duration_ms, portMAX_DELAY) == pdTRUE) {
            PORTB |= _BV(PB5);
            vTaskDelay(duration_ms / portTICK_PERIOD_MS);

            PORTB &= ~_BV(PB5);
            vTaskDelay(duration_ms / portTICK_PERIOD_MS);
        }
    }
}

static void vSerialTask(void* pvParameters) {
    xQueueHandle queue = (xQueueHandle)pvParameters;
    char input_buffer[8];
    uint16_t value;
    int i;
    char c = 0;

    uart_init();
    stdin = stdout = stderr = &uart_file;

    printf("\r\nEnter ms: ");

    for (;;) {
        i = 0;
        while (i < 7) {
            c = getchar();

            if (c == '\r' || c == '\n') {
                putchar('\r\n');
                break;
            }

            putchar(c);
            input_buffer[i] = c;
            i++;
        }

        input_buffer[i] = '\0';

        value = (uint16_t)atoi(input_buffer);

        if (value > 0 && value <= 100000) {
            xQueueSend(queue, &value, portMAX_DELAY);
            printf("\r\nOK\r\nEnter ms: ");
        } else {
            printf("\r\nERR\r\nEnter ms: ");
        }
    }
}
