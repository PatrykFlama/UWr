#include <FancyRectangle.hpp>
#include <RegularRectangle.hpp>
#include <PrinterLibrary.hpp>
#include "solution/FancyToRegular.hpp"
#include <Point.hpp>
#include <bits/stdc++.h>

int main()
{
    RegularRectangle regular(Point{.x = 8, .y = 9}, Point{.x = 10, .y = 11});
    FancyRectangle fancy(Point{.x = 9, .y = 10}, 2, 2);
    FancyToRegular fancyToRegular(&fancy);

    for(unsigned int i = 0; i < 3; i++)
    {
        print(regular);
        print(fancyToRegular);
        fancy.update();
        std::cout << std::endl;
    }
}
