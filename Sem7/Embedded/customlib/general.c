#pragma once

#include <avr/io.h>

#define RXTX_DISABLE() UCSR0B &= ~_BV(RXEN0) & ~_BV(TXEN0);
