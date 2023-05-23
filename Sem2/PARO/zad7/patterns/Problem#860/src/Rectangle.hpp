#pragma once
#include "Point.hpp"

#include <vector>

class Rectangle
{
public:
    virtual std::vector<Point> getCorners() const = 0;
};
