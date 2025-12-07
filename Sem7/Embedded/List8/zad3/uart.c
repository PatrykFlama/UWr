#include "FreeRTOS.h"
#include "task.h"
#include "queue.h"
#include <avr/io.h>
#include <avr/interrupt.h>
#include <stdio.h>
#include "uart.h"

#ifndef F_CPU
#define F_CPU 16000000UL
#endif
#ifndef BAUD
#define BAUD 9600
#endif
#include <util/setbaud.h>

#define UART_RX_QUEUE_SIZE 32
#define UART_TX_QUEUE_SIZE 32

static xQueueHandle uart_rx_queue = NULL;
static xQueueHandle uart_tx_queue = NULL;


static int uart_transmit(char c, FILE *stream);
static int uart_receive(FILE *stream);


FILE uart_file = FDEV_SETUP_STREAM(uart_transmit, uart_receive, _FDEV_SETUP_RW);


void uart_init() {
    uart_rx_queue = xQueueCreate(UART_RX_QUEUE_SIZE, sizeof(char));
    uart_tx_queue = xQueueCreate(UART_TX_QUEUE_SIZE, sizeof(char));

    UBRR0H = UBRRH_VALUE;
    UBRR0L = UBRRL_VALUE;
#if USE_2X
    UCSR0A |= _BV(U2X0);
#else
    UCSR0A &= ~(_BV(U2X0));
#endif
    UCSR0C = _BV(UCSZ01) | _BV(UCSZ00); /* 8-bit data */
    UCSR0B = _BV(RXEN0) | _BV(TXEN0);   /* Enable RX and TX */

    UCSR0B |= _BV(RXCIE0);
}



static int uart_transmit(char c, FILE *stream) {
  (void)stream; // unused

  xQueueSend(uart_tx_queue, &c, portMAX_DELAY);

  UCSR0B |= _BV(UDRIE0);

  return 0;
}

static int uart_receive(FILE *stream) {
  (void)stream; // unused
  char c;

  xQueueReceive(uart_rx_queue, &c, portMAX_DELAY);

  return (int)(unsigned char)c;
}


ISR(USART_RX_vect) {
  char c = UDR0;
  BaseType_t xHigherPriorityTaskWoken = pdFALSE;

  xQueueSendFromISR(uart_rx_queue, &c, &xHigherPriorityTaskWoken);
}

ISR(USART_UDRE_vect) {
  char c;
  BaseType_t xHigherPriorityTaskWoken = pdFALSE;

  if (xQueueReceiveFromISR(uart_tx_queue, &c, &xHigherPriorityTaskWoken) == pdTRUE) {
    UDR0 = c;
  } else {
    UCSR0B &= ~_BV(UDRIE0);
  }
}
