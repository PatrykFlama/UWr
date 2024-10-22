[(back)](../)

# TL;DR
- **Grid**: 19x10 torus (wraps around edges), with platforms, arrows, and voids.
- **Robots**: Move 1 cell/turn, change direction on arrows, stop if they:
  - Revisit same position/direction
  - Step off platforms
- **Arrows**: Place `U`, `R`, `D`, `L` on platform cells only.
- **Goal**: Score by keeping robots functional.


# The Goal
Score as many points as possible by guiding your robots on the platforms.

## Rules
You must guide multiple Automaton2000 robots by placing arrows on your first turn. Every turn, you score as many points as the number of Automaton2000 robots still functioning.

## The Map
The game is played on a grid 19 units wide and 10 units high.  
The grid is torus-shaped, meaning exiting the grid from one side will wrap you round to the other side.  
The coordinate `X=0, Y=0` is the top left cell.  
Each cell can be of 1 of 3 different types: platform, arrow, or void.

## Automaton2000
- Automaton2000 moves in a straight line by 1 cell every turn.
- Automaton2000 only changes direction when stepping on an arrow.
- Automaton2000 will stop functioning if it revisits a state (position + direction) it's been in.
- Automaton2000 will stop functioning if it steps off the platform.

## Arrows
- Arrows point to one of the 4 directions: `U` (Up), `R` (Right), `D` (Down), and `L` (Left).
- Arrows should be placed on empty platform cells only. Any other action will be ignored.

## Output
Place an arrow by outputting the command `X Y d` where `d` is the direction (`U`, `R`, `D`, or `L`) and `X, Y` are the coordinates of the cell.  
Place multiple arrows by separating your commands with a space.

## Details
### Order of events in a turn:
1. Score is incremented by the number of robots in function.
2. Automaton2000 robots move by 1 cell in the direction they're facing.
3. Automaton2000 robots change their direction if they're located on an arrow.
4. Automaton2000 robots stop functioning if they're located on a void cell or if they've entered a state (position, direction) they've been in before. (Automaton2000 robots don't share their state history)
5. During the first turn, Automaton2000 changes its direction if it's standing on an arrow (i.e., you can change the initial direction of a robot by placing an arrow under it).

## The referee code
The referee code of the game is accessible on GitHub: [CodinGame A-Star-Craft](https://github.com/CodinGameCommunity/A-Star-Craft).

## Note
Don’t forget to run the tests by launching them from the “Test cases” window. You can submit at any time to receive a score against the training validators.  
There are some limitations on submits per unit time.

Don't hesitate to change the viewer's options to help debug your code.

## Game Protocol
### Game Input
First 10 lines: a String line of 19 characters. Each character represents a cell:
- `#`: void cell
- `.`: empty platform cell
- `U`: platform cell with an Up arrow
- `R`: platform cell with a Right arrow
- `D`: platform cell with a Down arrow
- `L`: platform cell with a Left arrow

Next line: an Integer `robotCount`, the total number of Automaton2000 robots.

Next `robotCount` lines: for each Automaton2000 robot:
- 2 integers `x` and `y`, its coordinates,
- 1 character `direction`, the direction it faces (`U`, `R`, `D`, or `L`).

### Game Output
A single line containing triplets of the form `X Y DIR` with `X Y` two integers and `DIR` a character, either `U`, `R`, `D`, or `L`.

## Constraints
- `1 ≤ robotCount ≤ 10`
- Maximum characters allowed for one output: 10,000.
- Allotted response time to output is ≤ 1s.
