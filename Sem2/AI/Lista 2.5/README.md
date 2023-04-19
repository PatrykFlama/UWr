[(back)](../)
# List 2.5
| 1 | 2*| 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10|
|---|---|---|---|---|---|---|---|---|---|
| X | X | X | X | X | X | X | X | X | X |

## Problem 1
For every edge of the board and take maximum from `[distance of balck king from it]` and `[distance of white king to it - 2 cells]`, and take minimum from this maximum. That will give us minimal amount of steps that we have to take, to get kings to desired positions.\
Now for every cell by that edge (so for 8 cells) i calculate moves needed to get checkmate, and take min from that.\
If we introduce black tower to the game, we just have to calculate the same heuristics for black and white, and then take minimum from that (as tower of opponent wont disturb us in cooperative checkmate).\
So our heuristics is $min_{\textrm{cells by border}}(max(dist(\textrm{black king to cell}), dist(white king to black king)-1))$

## Problem 2*
For this one we take again only black king and from white figures king with 2 knights. We can notice that with such configuration we again can hava a checkmate only by the board border. \
So we can check all possible checkmate configurations, generally there is just one, thus we see that:
* black king has to be by the wall
* white king has to be 2 steps away from the wall, so 1 step apart from black king
* knights have to be 3 steps away from black king, but they can also move 3 steps at a time
  
So our heuristics function can be maximum from:
* $h_{\textrm{black king}} = dist(\textrm{closest wall})$
* $h_{\textrm{white king}} = dist(\textrm{black king}) - 1$
* $h_{\textrm{knight 1}} = (dist(\textrm{black king})-3)/3$
* $h_{\textrm{knight 2}} = (dist(\textrm{black king})-3)/3$

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

Optimistic but not consistent would be heuristic with all negative values. It could be helpful, as it still carries some information.

## Problem 4
$Optimistic \iff h \leq cost, f = h + dist$\
Lets take 2 ending states and analyze them. We will assume that $cost(s_1) < cost(s_2)$, and conduct a proof indirectly:\
Assuming that our heuristics found $s_2$ first $\implies$ $s_2$ was taken from queue before $s_1$ $\implies$ $f(s_2) < f(s_1)$\
Since heuristics is optimistic $h(s_2)=h(s_1)=0$, so $f(s_2)=dist(s_2)=cost(s_2)$ (same for $s_1$)\
Contradiction: $f(s_2) < f(s_1) \implies cost(s_2) < cost(s_1)$ and we assumed that $cost(s_1) < cost(s_2)$

## Problem 5
Preprocessing can be calculating distances from every city to goal, and from every city to city with pertol station.
Then we could simply traverse to cities closest to goal, if we have fuel, and otherwise to the closest petrol station. 

## Problem 6
With the 15 game the idea for good heuristic was to analyze all subgames of actual state:
* we find all subproblems for given state
* we take max cost from them as out heuristic
To use it with Sokoban we can do the same thing, where in subproblems we take out boxes from the board and calculate distance from box to the nearest goal. 

## Problem 7
In this problem we simply will move piece that is not in place to its destination, and piece that blocks the destination to empty cell.

## Problem 8
We can reduce operation simply by calculating all of its possible results and adding them to new domain, of new constraint which will replace that operation.

## Problem 9
[Helpful video](https://youtu.be/4cCS8rrYT14)\
AC3 ensures that we have arc consistency in our constraints.\
This algorithm has time complexity of $O(cd^3)$ and space complexity of $O(c)$, where _c_ is number of constraints, and _d_ size of domain.\
We know that our constraints look like this [operation on domains] $\circ$ [domain]\
To speed it up (space complexity doesn't change) we can alter a bit propagation of constraints - we modify all of them i such a way, that we use only $<=$ and $>=$. Now for every constraint we can binsearch the solution - calculate marginal case for left side and binsearch thru values of domain on the right side.\
Our new time complexity is $O(cd\log{d})$ becouse for every constraint we execute our algorithm, and we will add it to the queue max _d_ times, as thats how many elements it contains, $\log{d}$ is time for computing one case from queue.

## Problem 10
Differential heuristic: $h(n) = \max_{L \in Landmarks}(C^*(n, L) - C^*(L, goal))$ if negative retrurns _0_\
h(s) < real cost, which we can show using triangle inequality:\
lets take any landmark _L_, city _s_ and _goal_, for then our heuristic to be optimistic should: $C^*(n, L) - C^*(L, goal) \leq C^*(n, goal)$\
we can substitute equation with _a, b, c_: $b-c \leq a \implies a+c \geq b$, which as we can see (from picture) fulfills triangle inequality\
Now, better way to choose landmarks that just to randomize them, is to pick the furthes possible ones - as we can see our heuristic is best when $C^*(n, L)$ is big and $C^*(L, goal)$ is small. We can choose such points drawing convex hull around all points, and picking landmarks from the convex hull (first at random, then we choose by distance from other).\
That will help our heuristics, as it will increase its values.
