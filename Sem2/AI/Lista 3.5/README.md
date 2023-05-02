[(back)](../)
# List 3.5
| 1 | 2 | 3*| 4 | 5 | 6 | 7 | 8*| 9 | 10|11*|
|---|---|---|---|---|---|---|---|---|---|---|
| X | X | X | X |   |   | X | X |   |   |   |

## Exercise 1
<details>
    <summary>recap</summary>
    <img src="p0.png" width="50%" height="50%">
    <img src="p1.png" width="50%" height="50%">
    <img src="p2.png" width="50%" height="50%">
</details>

#### a) Local beam search _for k = 1_ (lect 6)
Since _local beam search_ is search algorithm that keeps _k_ best nodes and recursively looks for next _k_ best nodes from them, then for _k = 1_ it will keep only one best node.\ 
So _local beam search_ for _k = 1_ is the same as _hill climbing_.

#### b) Local beam search _with one state and no limit for saved states_
Having only one starting state and no limit for saved states, _local beam search_ will behave exactly like _BFS_.

#### c) Simulated annealing with T = 0 for the entire running time of the algorithm
Lets take _T = ε_, now $p=e^{\frac{ΔF}{T}}=e^{\frac{ΔF}{ε}} \xrightarrow ε 0$.\
So we have no chance of accepting worse solutions, which means that _simulated annealing_ will behave exactly like _hill climbing_.

#### d) Simulated annealing with T = ∞ for the entire running time of the algorithm
$p=e^{\frac{ΔF}{T}} \xrightarrow {T \xRightarrow{} \infty} \infty$\
Thus we will always accept worse solutions, which means that _simulated annealing_ will behave exactly like _random walk_.

#### e) Genetic algorithm with a population of size 1
Since we have only one individual, we can't perform crossover, so we can only mutate it.\
When we mutate an individual, we get his neighbour, and if it's better than him, we replace him with it.\
So _genetic algorithm_ with a population of size 1 is the same as _hill climbing_.\
(having population size _k_ is the same as _local beam search_ with _k_ saved nodes)

## Exercise 2
<details>
    <summary>recap</summary>
    <img src="p0.png" width="50%" height="50%">
    <img src="p3.png" width="50%" height="50%">
    <img src="p4.png" width="50%" height="50%">
</details>

#### a) Evolution algorithms + hill climbing
We can help our mutation process by using _hill climbing_ to find the best neighbour of our individual.\
That will speed up our search, and will be different from _hill climbing_ because we will still have some randomness in our search, thanks to crossover.

#### b) $A^*$ + local beam search
Basing from typical $A^*$, we will keep only _k_ best nodes and choose from them new _k_ best nodes. We gain time efficiency, but we lose optimality.

#### c) Simulated annealing (rough idea) + evolution algorithms
Simply we can decrease probability of mutation with time, which in some situations may help - after some time we should generate quite good individuals, so mutations won't be very effective (their changes are very subtle, in contrary to crossovers).

#### d) Taboo search + evolution algorithms
We can increase our search space, using _taboo search_ to save places that we already visited and generate new individuals further from them. Thus we will have quite broad exploration, thanks to whick better chance to get closer to global maxima - using evolution algorithm.

## Exercise 3*
Ant colony algorithm is probablistic method inspired by ants behaviour, that looks for 'better' paths to find the solution.\
Ants travel randomly, but when food is found they take it back to the nest, leaving pheromones on their way. Thanks to that it is more probable that other ants will follow the same path. Pheromones intensity matters, and it decreases with time. Thus shorter paths are more probable to be chosen.  
Travelling salesman problem:
* we have n cities
* we want to visit each city only once
* we want to minimize total distance travelled

Solution:
We spawn an ant in random city and let it travel randomly (each city only once), with probability to travel to city _n_ dependant on its weights.   
After travel we judge the score (total distance) and based on it and ants path we update pheromones on our graph.  
After _k_ iterations we end up with most probably shortest path.

## Exercise 4
Our variables are given blocks - *B_idx_len*\
Our domains are all possible blocks positions\
Our constraints are: for block - for _len_ steps ahead from its position there can't be any wall or block = for every block (in that row/col) its starting position is different

## Exercise 5
Our lessons have assigned classes and teachers, we have to assign them appropriate date (1-50)\
Our variables are lessons - *L_class_time*\
Our domains are all possible teachers\
Our constraints are:
* for every lesson in given class its starting time is different <=> L_C1_T != L_C2_T
* c

## Exercise 6


## Exercise 7, 8*
Jumpers  
Goal: get all jumpers to the other side of the board, we can only walk on generated roads (ours and opponents)  
Heuristics:  
* sum of distances of our pieces from the other side of the board (minimalize)  
* sum of distances of opponents pieces from the other side of the board (maximize)  

Pentago  
Goal: get 5 in a row (horizontally, vertically or diagonally)  
Heuristics: weighted sum of number of 4 in a row, number of 3 in a row, number of 2 in a row, number of 1 in a row - sums of opponent

## Exercise 9


## Exercise 10


## Exercise 11*


