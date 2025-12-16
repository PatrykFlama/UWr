#include <avr/io.h>
#include <util/delay.h>

#define LED_DDR DDRB
#define LED_PORT PORTB
#define LED_PIN PB2

#define BTN_DDR DDRA
#define BTN_PORT PORTA
#define BTN_PINR PINA
#define BTN_PIN PA7

#define MOSI_PIN PA4
#define SCK_PIN PA5
#define MISO_PIN PA6

static inline uint8_t btn_read() {
    return (BTN_PINR & _BV(BTN_PIN)) ? 0 : 1;
}

static inline void led_write(uint8_t on) {
    if (on) {
        LED_PORT |= _BV(LED_PIN);
    } else {
        LED_PORT &= ~_BV(LED_PIN);
    }
}

void spi_init()
{
    // ustaw piny MOSI i SCK jako wyjścia, MISO jako wejście
    DDRA |= _BV(MOSI_PIN) | _BV(SCK_PIN);
    DDRA &= ~_BV(MISO_PIN);
    // ustaw USI w trybie trzyprzewodowym (SPI)
    USICR = _BV(USIWM0);
}

uint8_t spi_transfer(uint8_t data)
{
    // załaduj dane do przesłania
    USIDR = data;
    // wyczyść flagę przerwania USI
    USISR = _BV(USIOIF);
    // póki transmisja nie została ukończona, wysyłaj impulsy zegara
    while (!(USISR & _BV(USIOIF))) {
        // wygeneruj pojedyncze zbocze zegarowe
        // zostanie wykonane 16 razy
        USICR = _BV(USIWM0) | _BV(USICS1) | _BV(USICLK) | _BV(USITC);
    }
    // zwróć otrzymane dane
    return USIDR;
}

int main()
{
    LED_DDR |= _BV(LED_PIN);
    BTN_DDR &= ~_BV(BTN_PIN);
    BTN_PORT |= _BV(BTN_PIN);

    spi_init();

    while (1) {
        uint8_t tx = btn_read();
        uint8_t rx = spi_transfer(tx);
        led_write(rx);
        _delay_ms(100);
    }
}

