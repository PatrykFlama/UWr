#include "color.hpp"
#include "pixel.hpp"

int main(){
    Color c(1, 2, 3);
    TransparentColor tc(4, 5, 6, 50);
    NamedColor nc(7, 8, 9, "name");
    TransNamColor tnc(1, 3, 6, 100, "coloredname");

    cout << c << '\n' << tc << '\n' << nc << '\n' << tnc << '\n';

    tnc.lighten(5);
    cout << tnc << '\n';
    tnc.darken(3);
    cout << tnc << '\n';

    Pixel p(1, 2);
    ColorPixel cp(3, 4, 5, 6, 7, 8);

    cout << p << '\n' << cp << '\n';

    cout << cp.calc_dist_left()    << '\n';
    cout << cp.calc_dist_right()   << '\n';
    cout << cp.calc_dist_up()  << '\n';
    cout << cp.calc_dist_down() << '\n';

    cp.transform(11, 22);
    cout << cp << '\n';

    cout << pixel_distance(p, cp) << ' ' << pixel_distance(&p, &cp) << '\n';
}
