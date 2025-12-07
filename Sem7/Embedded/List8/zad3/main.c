#include <avr/io.h>
#include <stdio.h>

#include "FreeRTOS.h"
#include "task.h"
#include "uart.h"

/******************************************************************************
 * Private macro definitions.
 ******************************************************************************/

#define mainIO_TASK_PRIORITY 2
#define mainBLINK_TASK_PRIORITY 1

/******************************************************************************
 * Private function prototypes.
 ******************************************************************************/

static void vSerialTask(void* pvParameters);
static void vBlinkTask(void* pvParameters);

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

int main(void) {
    xTaskHandle io_handle;
    xTaskHandle blink_handle;

    uart_init();
    stdin = stdout = stderr = &uart_file;

    xTaskCreate(
        vSerialTask,
        "io",
        configMINIMAL_STACK_SIZE + 64,
        NULL,
        mainIO_TASK_PRIORITY,
        &io_handle);

    xTaskCreate(
        vBlinkTask,
        "blink",
        configMINIMAL_STACK_SIZE,
        NULL,
        mainBLINK_TASK_PRIORITY,
        &blink_handle);

    vTaskStartScheduler();

    return 0;
}

void vApplicationIdleHook(void) {
}

/******************************************************************************
 * Private function definitions.
 ******************************************************************************/

static void vSerialTask(void* pvParameters) {
    int value;

    for (int i = 0; i < 10; i++) {
        printf("================================\r\n");
    }

    for (;;) {
        char buffer[32];
        scanf("%31s", buffer);
        printf("> %s\r\n", buffer);
    }
}

static void vBlinkTask(void* pvParameters) {
    DDRB |= _BV(PB5);
    PORTB &= ~_BV(PB5);

    for (;;) {
        PORTB |= _BV(PB5);
        vTaskDelay(500 / portTICK_PERIOD_MS);

        PORTB &= ~_BV(PB5);
        vTaskDelay(500 / portTICK_PERIOD_MS);
    }
}
