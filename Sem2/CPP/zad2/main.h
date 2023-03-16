#pragma once

#include <bits/stdc++.h>
using namespace std;

#ifndef DOUBLE_EPSILON
#define DOUBLE_EPSILON
const double double_epsilon = 0.000000000005;
#endif

#include "point.h"
#include "segment.h"
#include "triangle.h"
#include "line.h"
#include "vector.h"


double point_distance(Point a, Point b);
bool parallel(Segment s1, Segment s2);

bool perpendicular(Segment s1, Segment s2);

bool intersect_seg(Segment a, Segment b);
bool intersect(Triangle t1, Triangle t2);

bool contains(Triangle t1, Triangle t2);

bool does_not_intersect(Triangle t1, Triangle t2);

