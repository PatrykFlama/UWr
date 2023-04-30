[(back)](../)
# List 3.5
| 1 | 2 | 3*| 4 | 5 | 6 | 7 | 8*| 9 | 10|11*|
|---|---|---|---|---|---|---|---|---|---|---|
| X | X |   |   |   |   |   |   |   |   |   |

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


## Exercise 4


## Exercise 5
<!-- TODO lecture 5, ~15 -->

## Exercise 6


## Exercise 7


## Exercise 8*


## Exercise 9


## Exercise 10


## Exercise 11*


