#include <gtest/gtest.h>

#include <algorithm>
#include <iterator>
#include <vector>

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
    return {};
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
}

