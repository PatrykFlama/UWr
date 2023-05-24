// #include <gtest/gtest.h>

#include <algorithm>
#include <iterator>
#include <vector>
#include <bits/stdc++.h>

struct Product final
{
    float price;
    explicit Product(float price) : price(price) {}
    bool operator==(const Product& o) const { return price == o.price; }
    bool operator<(const Product& o) const { return price < o.price; }
    // Note: This class should not be touched.
};

std::vector<Product> find_cheapest(const std::vector<Product>& v, size_t num)
{
    // Funkcja ma zwrócić wektor zawierający [num] najtańszych produktów
    std::vector<Product> res1 = v;
    std::partial_sort(res1.begin(), res1.begin()+num, res1.end());
    std::vector<Product> res2(num);
    std::copy(res1.begin(), res1.begin()+num, res2.begin());
    return res2;
}

TEST(Cheap, Test)
{
    std::vector products{ Product{0.10}, Product{0.20}, Product{0.50}, Product{0.80}, Product{0.99},
                          Product{1.00}, Product{1.20}, Product{1.50}, Product{1.80}, Product{1.99},
                          Product{2.00}, Product{2.20}, Product{2.50}, Product{2.80}, Product{2.99} };

    auto const numCheapest = 5;
    std::vector<Product> expected;
    expected.reserve(numCheapest);
    std::copy(std::begin(products), std::begin(products) + numCheapest, std::back_inserter(expected));
    std::random_shuffle(products.begin(), products.end());
    auto actual = find_cheapest(products, numCheapest);
    std::sort(std::begin(actual), std::end(actual));

    ASSERT_EQ(expected, actual);
}

TEST(Cheap, TestCornerCase)
{
    std::vector products { Product{1.0} };

    auto const numCheapest = 5;
    std::vector expected { Product{1.0} };

    ASSERT_EQ(expected, find_cheapest(products, numCheapest));
}

TEST(Cheap, ShouldNotFailOnEmptyContainer)
{
    ASSERT_NO_THROW(find_cheapest(std::vector<Product>{}, 1));
}

int main(int ac, char ** av)
{
    ::testing::InitGoogleTest(&ac, av);
    return RUN_ALL_TESTS();
    // std::vector<int> test = {6, 23, 7, 2, 32,7, 1,464};
    // std::vector<int> res = find_cheapest(test, 4);
    // for(auto i : res) std::cout << i << ' ';
    // std::cout << '\n';
}

