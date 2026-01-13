#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <stdint.h>
#include "../hd44780/hd44780.h"


static void lcd_upload_progress_chars() {
	const uint8_t block_rows[6] = {
        0b00000,
        0b10000,
        0b11000,
        0b11100,
        0b11110,
        0b11111
	};

	for (uint8_t character = 0; character < 6; character++) {
		// CGRAM address: base = character << 3
		LCD_WriteCommand(HD44780_CGRAM_SET | (character << 3));
        // write 8 block_rows of pattern (last one cursor position)
		for (uint8_t r = 0; r < 8; r++) {
			LCD_WriteData(block_rows[character]);
		}
        // LCD_WriteData(0b00000); // cursor position as 0
	}
	// return to DDRAM addressing by setting any DDRAM address
	LCD_GoTo(0, 0);
}

static void lcd_draw_progress(uint8_t level, uint8_t row) {
	if (level > 80) level = 80;
	uint8_t full = level / 5;     // number of fully filled cells
	uint8_t rem  = level % 5;     // partial fill column

	for (uint8_t col = 0; col < 16; col++) {
		uint8_t ch;
		if (col < full) {
            // 5-column character (fully filled)
			ch = 5;
		} else if (col == full && rem != 0) {
            // partial fill character (1..4)
			ch = rem;
		} else {
            // empty character
			ch = 0;
		}
		LCD_GoTo(col, row);
		LCD_WriteData(ch);
	}
}

int main() {
	LCD_Initialize();
	LCD_Clear();
	lcd_upload_progress_chars();    
	
    uint8_t level = 0;
	while (1) {
		lcd_draw_progress(level, 0);
		level = (level + 1) % 81;
		_delay_ms(100);
	}
}

