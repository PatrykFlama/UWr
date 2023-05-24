#include "BowlingGame.hpp"


void BowlingGame::roll(int pins) {
    if(bonus[0]){
        score += pins*bonus[0];
        bonus[0] = bonus[1];
        bonus[1] = 0;
    }

    if((rounds-1)/2 == 0){      // last round
        if(rounds < 0 || (rounds == 0 && bonus == 0)) return;     // game over
        if(pins == 10) {    // strike
            score += pins;
            rounds--;
            // bonus[0]++, bonus[1]++;
        } else if(left_pins-pins == 0) {  // spare
            score += pins;
            rounds--;
            // bonus[0]++;
        } else{
            score += pins;
            rounds--;
            left_pins = 10 - pins;
        }
    } else{
        if(pins == 10) {    // strike
            score += pins;
            rounds -= 2;
            bonus[0]++, bonus[1]++;
        } else if(left_pins-pins == 0) {  // spare
            score += pins;
            rounds--;
            bonus[0]++;
        } else{
            score += pins;
            rounds--;
            left_pins = 10 - pins;
        }
    }

    if(rounds%2 == 0) left_pins = 10;   // reset left pins
}

int BowlingGame::getScore(){
    return score;
}
