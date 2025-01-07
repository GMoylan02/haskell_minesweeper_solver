#  Minesweeper game and solver with UI in Haskell

Includes a playable game of Minesweeper that runs with UI in the browser.
Also includes an auto solver.

# How to run
Simply compile and run the program, and then open `http://127.0.0.1:8023/`

# Solver
The solver is not perfectly accurate, but provided it gets a bit lucky with the first few guesses it should have a decent success rate.
It is a fairly naive implementation which repeatedly flags guaranteed mines and reveals guaranteed safe tiles, as well as simple 1-2-X pattern matching.
If left with no other options it will reveal a cell at random.
