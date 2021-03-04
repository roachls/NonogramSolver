# NonogramSolver
A Scala brute-force nonogram solver

This program was something I used to teach myself Scala. It solves black-and-white Nonogram puzzles using a brute-force method. There is a simple UI where you can see the progress.

To run:

NonogramSolver /path/to/puzzlefile

Puzzle files should be in the following format (see the sample_puzzles directory for some examples):

numRows
numCols
row-defs
col-defs

Where numRows and numCols are integers, and row-defs and col-defs are comma-separated lists of lists, with each row-list in quotes and commas between row-lists. Hopefully this
example will make some sense out of this:

30
30
"9,9","10,9","10,5,3","10,3,1","10,2,3","11,3","13,6","15,7","15,7","14,7","14,7","14,2,4","14,1,4","14,2,3","13,3,1","13,3","13,4","9,6,1","9,6,2","8,1,1,1,2,1","7,6,1","7,6","7,8","6,9","2,1,9","1,9","1,8","1,8","1,7","1,4,6"
"30","25","24,1","24,1","25,1","24,1","23","20,3,2","19,8","16,8","12,8","11,10","11,11","7,10","2,9","1,7","1,2,2","3","3","6","7","4,7","5","5,4","3,5","3,5","2,8","3,11","3,10,1","13,4"

This describes a 30x30 puzzle. The row 1 description is "9 9", row 2 is "10 9", etc. Column 1 is "30", column 2 is "25", column 3 is "24 1", etc.

In general the program is fairly fast. It runs significantly faster if you toggle GUI updating off with the "toggle updating" button.
