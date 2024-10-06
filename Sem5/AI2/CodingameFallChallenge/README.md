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

## Game loop sequence
1. City parsing == how the city looks like now, how many resources we have, what new buildings are there
2. Transport infrastructure improvement == player can build tubes, teleporters, or upgrade them
3. Passengers movement simulation == passengers move from building to building based on their ai
4. End of the lunar month == we get 10% of the unused resources

## Construction and costs
* `TUBE buildingId1 buildingId2`: create a magnetic tube between two buildings. The cost is **1 resource for each 0.1km** of tube installed, rounded down.
* `UPGRADE buildingId1 buildingId2`: increase the capacity of a tube by 1. You will need to spend the **initial construction cost multiplied by the new capacity**. For example, if a tube initially cost 500 resources to build, you have to spend 1000 resources to improve its capacity to 2 pods, then 1500 resources for 3 pods, etc.
* `TELEPORT buildingIdEntrance buildingIdExit`: build a teleporter. This action costs **5,000 resources**.
* `POD podId buildingId1 buildingId2 buildingId3 ...`: create a transport pod and define its path. The pod identifier must be included between 1 and 500. If the last stop is the same as the first, the pod will loop around the path indefinitely, otherwise it will remain in place after reaching its last stop. This action costs **1,000 resources**.
* `DESTROY podId`: deconstruct a transport pod. This action gives you **750 resources back**.
* `WAIT`: don't perform any action this turn.

> In order to execute several actions during the same game turn, you can separate them with a semicolon like this: `TELEPORT 12 34;TUBE 23 45;UPGRADE 23 45`.
