#include <FancyRectangle.hpp>
#include <RegularRectangle.hpp>
#include <PrinterLibrary.hpp>
#include <Point.hpp>

int main()
{
    RegularRectangle regular(Point{.x = 8, .y = 9}, Point{.x = 10, .y = 11});
    FancyRectangle fancy(Point{.x = 9, .y = 10}, 2, 2);

    for(unsigned int i = 0; i < 3; i++)
    {
        print(regular);
        print(fancy);
        fancy.update();
        std::cout << std::endl;
    }
}
