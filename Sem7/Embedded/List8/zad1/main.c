/******************************************************************************
 * Header file inclusions.
 ******************************************************************************/

#include "./freertos_atmega328P/FreeRTOS/Source/include/FreeRTOS.h"
#include "./freertos_atmega328P/FreeRTOS/Source/include/task.h"

#include <avr/io.h>
#include <stdio.h>
#include <string.h>

/******************************************************************************
 * External declarations.
 ******************************************************************************/

#define mainBUTTON_TASK_PRIORITY    2
#define mainLED_SHIFT_TASK_PRIORITY 2

#define BTN_PIN PB4
#define BTN_PORT PORTB
#define BTN_PIN_REG PINB

#define LED_SHIFT_PORT PORTD
#define LED_SHIFT_DDR DDRD

#define BUTTON_BUFFER_SIZE 10
#define BUTTON_POLL_DELAY_MS 100
#define LED_SHIFT_STEP_DELAY_MS 300

// (10 samples * 100ms = 1 second)
uint8_t button_buffer[BUTTON_BUFFER_SIZE];
uint8_t buffer_index = 0;

/******************************************************************************
 * Private function prototypes.
 ******************************************************************************/

static void vButtonTask(void* pvParameters);

static void vLedShiftTask(void* pvParameters);

/******************************************************************************
 * Public function definitions.
 ******************************************************************************/

/**************************************************************************//**
 * \fn int main(void)
 *
 * \brief Main function.
 *
 * \return
 ******************************************************************************/
int main(void)
{
    // Create tasks
    xTaskHandle button_handle;
    xTaskHandle led_shift_handle;
    xTaskHandle serial_handle;

    xTaskCreate
        (
         vButtonTask,
         "button",
         configMINIMAL_STACK_SIZE,
         NULL,
         mainBUTTON_TASK_PRIORITY,
         &button_handle
        );

    xTaskCreate
        (
         vLedShiftTask,
         "led_shift",
         configMINIMAL_STACK_SIZE,
         NULL,
         mainLED_SHIFT_TASK_PRIORITY,
         &led_shift_handle
        );

    // Start scheduler.
    vTaskStartScheduler();

    return 0;
}

/**************************************************************************//**
 * \fn static vApplicationIdleHook(void)
 *
 * \brief
 ******************************************************************************/
void vApplicationIdleHook(void)
{

}

/******************************************************************************
 * Private function definitions.
 ******************************************************************************/

/**************************************************************************//**
 * \fn static void vButtonTask(void* pvParameters)
 *
 * \brief Button polling task. Records button states in circular buffer.
 *        Updates LED with state from 1 second ago (10 samples * 100ms).
 *
 * \param[in]   pvParameters
 ******************************************************************************/
static void vButtonTask(void* pvParameters) {
    BTN_PORT |= _BV(BTN_PIN); // pull-up on button
    DDRB |= _BV(PB5); // LED as output

    for ( ;; ) {
        // read button state
        uint8_t curr_state = (BTN_PIN_REG & _BV(BTN_PIN)) ? 1 : 0;
        button_buffer[buffer_index] = curr_state;

        // get delayed state
        uint8_t delayed_index = (buffer_index + 1) % BUTTON_BUFFER_SIZE;
        uint8_t delayed_state = button_buffer[delayed_index];

        if (delayed_state)
            PORTB &= ~_BV(PB5);
        else
            PORTB |= _BV(PB5);

        buffer_index = (buffer_index + 1) % BUTTON_BUFFER_SIZE;

        // poll every 100ms
        vTaskDelay(BUTTON_POLL_DELAY_MS / portTICK_PERIOD_MS);
    }
}

/**************************************************************************//**
 * \fn static void vLedShiftTask(void* pvParameters)
 *
 * \brief LED shift effect task. Creates a moving light pattern on PORTD.
 *
 * \param[in]   pvParameters
 ******************************************************************************/
static void vLedShiftTask(void* pvParameters) {
    // portD output
    LED_SHIFT_DDR = 0xFF;

    uint8_t pattern = 0b00000001;
    uint8_t dir_left = 1;

    for ( ;; ) {
        LED_SHIFT_PORT = pattern;

        vTaskDelay(LED_SHIFT_STEP_DELAY_MS / portTICK_PERIOD_MS);

        pattern = dir_left ? (pattern << 1) : (pattern >> 1);

        if (pattern == 0b10000000 || pattern == 0b00000001) {
            dir_left = 1 - dir_left;
        }
    }
}
