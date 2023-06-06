#include "gtest/gtest.h"
#include "day-of-year.hpp"

struct DayOfYearTestSuite {};

TEST(LeapTest, dummyTest)
{
  ASSERT_EQ(dayOfYear(3, 1, 1700), 28+31+1);
}
TEST(LeapTest1, dummyTest1)
{
  ASSERT_EQ(dayOfYear(3, 1, 2004), 28+31+1+1);
}
TEST(LeapTest2, dummyTest2)
{
  ASSERT_EQ(dayOfYear(3, 1, 2012), 28+31+1+1);
}
TEST(LeapTest3, dummyTest3)
{
  ASSERT_EQ(dayOfYear(3, 1, 2014), 28+31+1);
}

TEST(DayOfYearTestSuite, January1stIsFitstDayOfYear)
{
  ASSERT_EQ(dayOfYear(1, 1, 2020), 1);
}

