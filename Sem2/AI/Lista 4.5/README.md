[(back)](../)
# List 4.5
| 1 | 2*| 3 | 4 | 5 | 6 | 7 | 8 | 9*|10*|
|---|---|---|---|---|---|---|---|---|---|
| X |   | X | X | X | X | X |   | X |   |


# Exercise 1
_Null move heuristic_ is enchancement for alpha beta pruning, which allows players to make null move.  
When player has significantly higher score he can make null move, essentially allowing opponent to play again. If opponent couldn't even the score we can consider that position as winning one.  
Same thing happens if the opponent has higher score and we are unable to even the score after additional free move - that means that this state is most probably loosing and we dont have to evaluate it more.  

# Exercise 3
## a)
Drawing random state in this case most probably means drawing random deals for players, which deck we don't know.

## b)
During the bidding opponents decide what type of playthrought is most profitable for them. That takes in account colors and overall strength of the hand. Thus we could assume which cards do they more probable have and which do they not, putting weights on them during random deck generation.  
But that creates another problem - opponent can be bluffing during the bidding or simply can try to communicate with his partner creating false impression about his hand.

## c)
In such simulation we ignore a lot of small details, that are quite frequent in real life. For example bluffing, cheating, cooperation not provided for by the rules of the game, assuming that opponent is playing optimally, etc. Those factors can have significant impact on the game, but are hard to simulate.  
Also poker has some stuff in it. <!-- TODO -->

# Exercise 4
Agents for cheat game (better than random)  
* Agent 1  
First thing that comes to mind is simply player that counts cards, to which he is sure that are in the game. When opponent declares impossible amount of cards we can assume that he is cheating and can check him.

* Agent 2  
Another simple improvement would be our play style - how we put the cards on the stack. We can always, if its possible, put without cheating the lowest possible cards up to some set figure (that we precalculatet to be optimal, lets say that thats 8). Then we would save the more powerful cards for later and in another range (precalculated - 9 Q) try to blend in the lowest cards that we have. For cards above that range we play normally, if possible.

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

# Exercise 7
We can represent any boolean function with neural network. Bollean function takes _n_ arguments and returns binary output. To represent such function we actually need 1 layer of neurons (up to $2^n$). In the worst case, considering input very diverse, we could assing every input combination to neuron weighting the input appropriately.

# Exercise 9
_Superrationality_ is a concept in game theory, which states that the opponent performs identical reasoning as we do, and goes for the best result that it gives with such assumption.
  
### Example on the prissoner's dilemma:
The most known version of prissoner's dilemma is based on sentences for criminals, but in the example from [Wikipedia](https://en.wikipedia.org/wiki/Superrationality) cash prizes are used instead.  
Two players have choice to cooperate or defect without communicating. If one of them defects the other one will get $200, if both deflect they will get $1, and if noone deflect both get $100.  
These are the four possible outcomes:  

|                       | Player B cooperates                | Player B defects                   |
|-----------------------|------------------------------------|------------------------------------|
|**Player A cooperates**| Both get $100	                     | Player A: $0 <br /> Player B: $200 |
|**Player A defects**   | Player A: $200 <br /> Player B: $0 | Both get $1                        |

Based on the table, the **rational player** (the rational thing to do) is to **defect**, as follows:  
1. If other player defects, I will get nothing cooperating and $1 defecting
2. If other player cooperates, I will get $100 cooperating and $200 defecting
3. So independantly from opponent player choice, my payoff is increased by defecting.

But **superrational players** would assume that opponent is going thru the same thought process and would see that most profitable for them is when they will both **cooperate**, choosing that option.  

# Exercise 10
[Nice link, probably corelated with the topic](https://ncase.me/trust/)
