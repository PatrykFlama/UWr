#include "catch.hpp"
#include "BowlingGame.hpp"
#define FOR(a) for(int i = 0; i < a; i++)

// TEST_CASE("failed_succesfully"){
//   REQUIRE(true == false);
// }

// we assume that game is played correctly
TEST_CASE("CheckStartingScore"){
  BowlingGame game;
  REQUIRE(game.getScore() == 0);
}
TEST_CASE("NoKnockedPins"){
  BowlingGame game;
  for(int i = 0; i < 20; i++) game.roll(0);
  REQUIRE(game.getScore() == 0);
}
TEST_CASE("StrikeStart"){
  BowlingGame game;
  game.roll(10);
  SECTION("NoSpare"){
    game.roll(1);
    game.roll(2);
    game.roll(3);
    REQUIRE(game.getScore() == (10+1*2+2*2+3));
  }
  SECTION("Spare"){
    game.roll(8);
    game.roll(2);
    game.roll(2);
    game.roll(1);
    REQUIRE(game.getScore() == (10+8*2+2*2+2*2+1));
  }
}
TEST_CASE("Game3LastThrows"){
  BowlingGame game;
  int rolls[] = {2,3,4,6,2,6,1,4,2,6,2,7,5,0,6,4,10,7,3,10};
  for(auto i : rolls) game.roll(i);
  REQUIRE(game.getScore() == 112);
}
TEST_CASE("RandomGame"){
  BowlingGame game;
  SECTION("RandomGame"){
    int rolls[] = {4,3,9,1,1,4,4,6,0,3,7,3,5,5,3,2,3,4,4,1};
    for(auto i : rolls) game.roll(i);
    REQUIRE(game.getScore() == 81);
  }
  SECTION("RandomGameNoEnding"){
    int rollsNoend[] = {4,3,9,1,1,4,4,6,0,3,7,3,5,5,3,2,3,4};
    for(auto i : rollsNoend) game.roll(i);
    REQUIRE(game.getScore() == 76);
  }
}
TEST_CASE("PerfectGame"){
  BowlingGame game;
  FOR(9) game.roll(10);
  SECTION("NoLastRound"){
    REQUIRE(game.getScore() == 240);
  }
  SECTION("NoLastThrow"){
    game.roll(10);
    REQUIRE(game.getScore() == 270);
  }
  SECTION("NoLast2Throws"){
    game.roll(10);
    game.roll(10);
    REQUIRE(game.getScore() == 290);
  }
  SECTION("PerfectGame"){
    game.roll(10);
    game.roll(10);
    game.roll(10);
    REQUIRE(game.getScore() == 300);
  }
}
TEST_CASE("Game9/1"){
  BowlingGame game;
  FOR(10){
    game.roll(9);
    game.roll(1);
  }
  game.roll(9);
  REQUIRE(game.getScore() == 190);
}