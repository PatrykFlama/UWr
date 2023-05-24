#pragma once
#include "Rectangle.hpp"
#include "FancyRectangle.hpp"
#include "RegularRectangle.hpp"

class FancyToRegular : public Rectangle {
    FancyRectangle *fancy;
public:
    FancyToRegular(FancyRectangle *_fancy) {
        fancy = _fancy;
    }

    std::vector<Point> getCorners() const override {
        return {fancy->middle - Point{.x = fancy->width/2, .y = fancy->width/2}, 
                fancy->middle + Point{.x = fancy->width/2, .y = fancy->height/2}};
    }
};

