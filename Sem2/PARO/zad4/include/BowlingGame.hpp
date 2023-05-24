#pragma once

class BowlingGame{
public:
	void roll(int pins);
	int getScore();
private:
	int rounds = 20;
	int score = 0;
	int bonus[2] = {0,0}; 	// bonus for next 2 throws
	int left_pins = 10;
};

