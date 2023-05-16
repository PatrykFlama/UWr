#include <bits/stdc++.h>
using namespace std;

/*
'.' - land
'#' - trap
'*' - den
'~' - water
-------------
R - rat
C - cat
D - dog
W - wolf
J - jaguar
T - tiger
L - lion
E - elephant
----/\/\/\----
animal power is from low to high, except Rat can eat Elephant
animal can beat same or less power animal, except animal in #trap
only Rat can go into the ~water
Rat cant beat from ~water to .land
only Tiger and Lion can jump over ~water (not instanteously, and not over the Rat)
----------------
animal abilities groups:
1. Rat
2. Cat, Dog, Wolf, Jaguar
3. Tiger, Lion
4. Elephant
*/

const char board[9][7] = {
    {'.', '.', '#', '*', '#', '.', '.'},
    {'.', '.', '.', '#', '.', '.', '.'},
    {'.', '.', '.', '.', '.', '.', '.'},
    {'.', '~', '~', '.', '~', '~', '.'},
    {'.', '~', '~', '.', '~', '~', '.'},
    {'.', '~', '~', '.', '~', '~', '.'},
    {'.', '.', '.', '.', '.', '.', '.'},
    {'.', '.', '.', '#', '.', '.', '.'},
    {'.', '.', '#', '*', '#', '.', '.'}};


class Jungle {
    vector<pair<int, int>> player_pieces;   // positions of R C D W J T L E; -1 -1 if eaten (rat can eat elephant)
    vector<pair<int, int>> enemy_pieces;
};