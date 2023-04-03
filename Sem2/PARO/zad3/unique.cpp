#include <gtest/gtest.h>

#include <algorithm>
#include <vector>

#include <unordered_set>
#include <ranges>

template <typename T>
std::vector<T> unique(const std::vector<T>& first, const std::vector<T>& second)
{
    // This function should join input vectors and return only unique elements.
    // Important: Use only containers! No algorithms allowed.
    std::set<T> sres(first.begin(), first.end());
    std::copy(second.begin(), second.end(), inserter(sres, sres.end()));
    std::vector<T> vres(sres.begin(), sres.end());
    return vres;
}

TEST(DuplicatesTest, Test)
{
    std::vector d1 = {1.1, 2.1, 3.1, 1.0, 1.1, 0.56, 0.44, 4.445, 0.001, 9.996, 0.001};
    std::vector d2 = {0.001, 0.002, 0.0003, 1.1, 0.44, 0.99, 0.996, 3.1, 3.12};

    std::vector expected = {1.0, 0.002, 1.1, 0.0003, 2.1, 0.44, 0.001, 0.996, 4.445, 3.1, 9.996, 0.56, 3.12, 0.99};
    auto actual = unique(d1, d2);
    std::sort(std::begin(expected), std::end(expected));
    std::sort(std::begin(actual), std::end(actual));
    ASSERT_EQ(expected, actual);
}

int main(int ac, char ** av)
{
    ::testing::InitGoogleTest(&ac, av);
    return RUN_ALL_TESTS();
}

