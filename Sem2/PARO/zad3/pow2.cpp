#include <gtest/gtest.h>

#include <algorithm>
#include <vector>
#include<cmath>
#include<bits/stdc++.h>

template <typename T>
std::vector<T> pow2(const std::vector<T>& input)
{
    // Funkcja zwraca wektor elementów podniesionych do potęgi 2.
    std::vector<T> res;
    std::transform(input.begin(),
                   input.end(),
                   std::back_inserter(res),
                   [](auto i){return i*i;});
    return res;
}

TEST(Pow2Test, Test)
{
    std::vector<long> input =    {2, 5,  10,  15,  20,  25,  30,  45,   100};
    std::vector<long> expected = {4, 25, 100, 225, 400, 625, 900, 2025, 10000};
    auto actual = pow2(input);

    ASSERT_EQ(expected, actual);
}

int main(int ac, char ** av)
{
    ::testing::InitGoogleTest(&ac, av);
    return RUN_ALL_TESTS();
    // std::vector<int> test = {1, 2, 3, 4};
    // auto res = pow2(test);
    // for(auto i : res){
        // std::cout << i << ' ';
    // }
    // std::cout << '\n';
}
