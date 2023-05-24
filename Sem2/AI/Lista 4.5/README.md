[(back)](../)
# List 4.5
| 1 | 2 | 3*| 4 | 5 | 6 | 7 | 8*| 9 | 10|
|---|---|---|---|---|---|---|---|---|---|
| X |   |   |   | X | X |   |   |   |   |


# Exercise 1
_Null move heuristic_ is enchancement for alpha beta pruning, which allows players to make null move.  
When player has significantly higher score he can make null move, essentially allowing opponent to play again. If opponent couldn't even the score we can consider that position as winning one.  
Same thing happens if the opponent has higher score and we are unable to even the score after additional free move - that means that this state is most probably loosing and we dont have to evaluate it more.  

# Exercise 3
## a)
Drawing random state in this case most probably means drawing random deals for players, which deck we don't know.

## b)


## c)


# Exercise 5
## a)
For every dataset that can be represented as linear combination of input features:  
For 2 groups, as it can be represented by $x+y$  
For checker (xor), as it can be represented by $xy$  
For circle, as it can be represented by $x^2 + y^2$  

## b)
The network quickly converges to nonoptimal result, becouse we enforce network to learn in too quick pace, so it is unable to fine-tune the result

## c)
For the spiral, becouse it shows some kind of periodicity.   

## d)
* for circle - 1 hidden layer with 3 neurons
* for checker (xor) - 1 hidden layer with 4 neurons
* for 2 groups - no hidden layers

# Exercise 6
* $x \lor y$  
$\sigma (x + y)$  
$\sigma (1*x + 1*y + 0)$  


* $x \land y$  
$\sigma (x + y - 1)$  
$\sigma (1*x + 1*y - 1)$  

* $\lnot x$  
$\sigma (1-x)$
$\sigma (-1*x + 1)$  

* $x \oplus y \equiv (x \lor y) \land \lnot (x \land y)$  
$\sigma (\sigma (x + y) + \sigma (1-\sigma (x + y - 1)) - 1)$  
$\sigma (1*x' + 1*y' - 1)$  
With additional neurons beeing:  
$x' = \sigma (1*x + 1*y + 0)$  
$y' = \sigma (-1*z' + 1)$  
$z' = \sigma (1*x + 1*y - 1)$  

Plotting xor function we can see that we are unable to separate it using just one line - to do it we need to combine 4 lines.  

