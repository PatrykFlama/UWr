#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>


#define LED_PIN   PB2
#define BTN_PIN   PB1

#define LED_DDR   DDRB
#define LED_PORT  PORTB
#define LED_PINR  PINB

#define BTN_DDR   DDRB
#define BTN_PORT  PORTB
#define BTN_PINR  PINB

#define SAMPLE_HZ      100
#define DELAY_SAMPLES  100
#define BUF_SIZE       (DELAY_SAMPLES)

static volatile uint8_t buffer[BUF_SIZE];
static volatile uint8_t buffer_ptr = 0;

static inline uint8_t btn_read() {
	return (BTN_PINR & _BV(BTN_PIN)) ? 1 : 0;
}

static inline void led_write(uint8_t on) {
	if (on) {
		LED_PORT |= _BV(LED_PIN);
	} else {
		LED_PORT &= ~_BV(LED_PIN);
	}
}

ISR(TIM0_COMPA_vect) {
	buffer[buffer_ptr] = btn_read();

    // read old value as set it
	uint8_t next_ptr = (buffer_ptr + 1) % BUF_SIZE;
	uint8_t val = buffer[next_ptr];
	led_write(val == 0 ? 1 : 0);

	buffer_ptr = next_ptr;
}

// generate ISR every 10 ms
static void timer0_init() {
    const uint8_t test_OCR0A = (F_CPU / (256UL * SAMPLE_HZ)) - 1;

	// CTC mode: WGM01 = 1
	TCCR0A |= _BV(WGM01);
	// set compare value
	OCR0A = test_OCR0A;
	// enable Output Compare A Match interrupt
	TIMSK0 |= _BV(OCIE0A);
	// set prescaler to clk/256 (CS02:0 = 0b100)
	TCCR0B |= _BV(CS02);
}

int main() {
	for (uint8_t i = 0; i < BUF_SIZE; ++i) buffer[i] = 1;

	// LED output
	LED_DDR |= _BV(LED_PIN);
	// Button input with pull-up
	BTN_DDR &= ~_BV(BTN_PIN);
	BTN_PORT |= _BV(BTN_PIN);

	timer0_init();
	set_sleep_mode(SLEEP_MODE_IDLE);
	sei();

	while (1) {
		sleep_mode();
	}
}

