[back](../)

# Diamond problem
[Neat article](https://isocpp.org/wiki/faq/multiple-inheritance#mi-diamond)\
Actually code for this exercise is quite messy, becouse of badly made constructors - it would be better if all of the parent constructor calls would be at the main constructor initializer, not inside it, so there would not be need for ugly separate empty constructors and code would be cleaner