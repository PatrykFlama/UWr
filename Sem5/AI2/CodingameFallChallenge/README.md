[(back)](../)

# Codingame Fall Challenge
## Game rules
20 months, 20 days each  
connect buildings efficiently with tubes or teleporters  

### Tubes
* each tube can transport `level` amout of pods per day  
* each pod can transport **10 passengers**
* each building can have up to **5 tubes**
* tubes cant cross each other

### Teleporters
* each teleporter can transport **one-way**  
* each building can have only **one teleporter** (entrance or exit)  

### Ladning pads
at the beginning of each month, **up to 20 passengers** arrive at the landing pads  
passenger configuration at each landing pad does not change for the whole game  

## Scoring
each passenger can give up to **100 points**:
* up to 50 for **travel time** (each day reduces the score by 1)
* up to 50 for **population balancing** (score reduced by 1 for each passenger that already arrived to the same building)

also at the end of the month we get 10% of the unused resources  

## Time restrictions
_500ms_ per turn, _1000ms_ for first turn
