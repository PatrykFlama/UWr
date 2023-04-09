[(back)](../)
# List 2.5
| 1 | 2*| 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10|
|---|---|---|---|---|---|---|---|---|---|
| X |   | X |   | ~ | ~ |   | ~ | X |   |

## Problem 1
For every edge of the board and take maximum from (distance of balck king from it) and (distance of white king to it - 2 cells), and take minimum from this maximum. That will give us minimal amount of steps that we have to take, to get kings to desired positions. \
Now for every cell by that edge (so for 8 cells) i calculate moves needed to get checkmate, and take min from that.\
If we introduce black tower to the game, we just have to calculate the same heuristics for black and white, and then take minimum from that (as tower of opponent wont disturb us in cooperative checkmate).

## Problem 2*


## Problem 3
Assuming that our heuristics is also reasonable (h(goal) = 0), as otherwise consistent wouldn't imply optimistic, so we know from definition that:
* $h(goal) = 0$
* $cost(a, b) + h(b) >= h(a)$

and we want to show that $h(a) <= cost(a)$. We will use induction:
1. Base:\
$h(goal) = 0 = cost(goal, goal)$
2. Step:\
Lets assume that $h(s_n) <= cost(s_n)$\
based on consistency of our heuristics:\
$h(s_{n+1}) <= cost(s_n, s_{n+1}) + h(s_n) <= cost(s_n, s_{n+1}) + cost(s_n) = cost(s_{n+1})$\
so\
$h(s_{n+1}) <= cost(s_{n+1})$

## Problem 4


## Problem 5
Preprocessing for first problem:\
we calculate distances from every city to goal, and from every city to city with pertol station.
The preprocessing that we can do for our heuristics is precalculating distances from cities to goal from every node and to closest city with petrol station from every node.\
For problem with packages we can calculate distance to closest city, in which we have a delivery to make, from every node.

## Problem 6
With the 15 game the idea for good heuristic was to analyze all subgames of actual state:
* we find all subproblems for given state
* we take max cost from them as out heuristic
To use it with Sokoban we can do the same thing, where in subproblems we take out boxes from the board and calculate distance from box to the nearest goal. 

## Problem 7


## Problem 8
We can reduce operation simply by calculating all of its possible results and adding them to new domain, of new constraint which will replace that operation.

## Problem 9
[Helpful video](https://youtu.be/4cCS8rrYT14)\
This algorithm has time complexity of $O(cd^3)$ and space complexity of $O(c)$, where _c_ is number of constraints, and _d_ size of domain.\
We know that our constraints look like this [operation on domains] $\circ$ [domain]\
To speed it up (space complexity doesn't change) we can alter a bit propagation of constraints - we modify all of them i such a way, that we use only $<=$ and $>=$. Now for every constraint we can binsearch the solution - calculate marginal case for left side and binsearch thru values of domain on the right side.\
Our new time complexity is $O(cd^2\log{d})$

## Problem 10

