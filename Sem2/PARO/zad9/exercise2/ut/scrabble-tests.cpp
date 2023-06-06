#include "gtest/gtest.h"
#include "scrabble.hpp"
#include <bits/stdc++.h>

struct ScrabbleTestSuite {};

TEST(Scrabble, t1)
{
  ASSERT_EQ(calculateScore("AAA"), 3);
}
TEST(Scrabble2, t2)
{
  ASSERT_EQ(calculateScore("BBB"), 3*3);
}
TEST(Scrabble3, t3)
{
  ASSERT_EQ(calculateScore("C"), 3);
}
