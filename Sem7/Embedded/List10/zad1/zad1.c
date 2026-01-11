#include <avr/io.h>
#include <util/delay.h>
#include <stdio.h>
#include <stdint.h>
#include "../hd44780/hd44780.h"
#include "../../customlib/uart.c"


static void lcd_enable_cursor() {
	LCD_WriteCommand(HD44780_DISPLAY_ONOFF |
					 HD44780_DISPLAY_ON |
					 HD44780_CURSOR_ON |
					 HD44780_CURSOR_BLINK);
}

static void render_line(uint8_t row, const char *buf) {
	LCD_GoTo(0, row);
	for (uint8_t col = 0; col < 16; col++) {
		LCD_WriteData(buf[col]);
	}
}

static void scroll_up(char lines[2][16]) {
	for (uint8_t i = 0; i < 16; i++) {
		lines[0][i] = lines[1][i];
		lines[1][i] = ' ';
	}
	render_line(0, lines[0]);
	render_line(1, lines[1]);
}

int main() {
	LCD_Initialize();
	LCD_Clear();
	lcd_enable_cursor();

	uart_init();


	char lines[2][16];
	for (uint8_t r = 0; r < 2; r++)
		for (uint8_t c = 0; c < 16; c++)
			lines[r][c] = ' ';
	render_line(0, lines[0]);
	render_line(1, lines[1]);

	uint8_t cursor_row = 1;
	uint8_t cursor_col = 0;
	LCD_GoTo(cursor_col, cursor_row);

	while (1) {
		int ch = getchar();

        if (ch == '\r') continue;
		if (ch == '\n') {
			scroll_up(lines);
			cursor_row = 1;
			cursor_col = 0;
			LCD_GoTo(cursor_col, cursor_row);
			continue;
		}

        lines[cursor_row][cursor_col] = (char)ch;
        LCD_GoTo(cursor_col, cursor_row);
        LCD_WriteData((unsigned char)ch);

        cursor_col++;
        if (cursor_col >= 16) {
            scroll_up(lines);
            cursor_row = 1;
            cursor_col = 0;
        }
        LCD_GoTo(cursor_col, cursor_row);
	}
}

