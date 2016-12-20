/**
* Solver for Board #1 is implemented here
*/

/* board d = 3 */
board1([[1, 3, 0, 1, 2],
       [3, 2, 0, 1, 3],
       [3, 3, 0, 0, 1],
       [2, 2, 1, 2, 0]]).

/* original board width */
boardWidth1(5).

/* length of each line in $Solution */
lineLength1(LineLength):-
  boardWidth1(Width),
  LineLength is 2 * Width + 1.

/* set of available dominos (replace 0 with 7 and 1 with 8) */
dominos1([7-7, 7-8, 7-2, 7-3, 8-8, 8-2, 8-3, 2-2, 2-3, 3-3]).


/* --- SOLVER --- */

board1Solver:-

  /* --- DECLARING DOMAIN VARIABLES --- */

  /**
  * This a representation of the Board, along with Domino Borders.
  * This representation is a simple list (NOT a matrix!)
  *
  * Eyx - Eastern Border of the square (y, x) on the Board
  * Syx - Southern Border of the square (y, x) on the Board.
  * A - Throw-away variable. It's only here to make all "lines" the same length.
  * Consider it a representation of the corners. (notice that since this is NOT a matrix, there are in fact no "lines")
  * [2, 6] - These numbers represent the amount of dots (of the half-domino) on that square
  * 7 - Half-domino with 0 dots
  * 8 - Half-domino with 1 dot
  */
  Solution = [ A, S01, A, S02, A, S03, A, S04, A, S05, A,
              E10, 8, E11, 3, E12, 7, E13, 8, E14, 2, E15,
               A, S11, A, S12, A, S13, A, S14, A, S15, A,
              E20, 3, E21, 2, E22, 7, E23, 8, E24, 3, E25,
               A, S21, A, S22, A, S23, A, S24, A, S25, A,
              E30, 3, E31, 3, E32, 7, E33, 7, E34, 8, E35,
               A, S31, A, S32, A, S33, A, S34, A, S35, A,
              E40, 2, E41, 2, E42, 8, E43, 2, E44, 7, E45,
               A, S41, A, S42, A, S43, A, S44, A, S45, A ],

  /**
  * A list containing ALL Eastern and Southern Borders declared above.
  *
  * While $Solution is used to set the constraints,
  * $Borders is used to set the domain of the variables.
  */
  Borders = [      S01, S02, S03, S04, S05,
              E10, E11, E12, E13, E14, E15,
                   S11, S12, S13, S14, S15,
              E20, E21, E22, E23, E24, E25,
                   S21, S22, S23, S24, S25,
              E30, E31, E32, E33, E34, E35,
                   S31, S32, S33, S34, S35,
              E40, E41, E42, E43, E44, E45,
                   S41, S42, S43, S44, S45 ],

  /* --- ! DECLARING DOMAIN VARIABLES --- */


  /* --- SPECIFYING DOMAINS --- */

  /**
  * Domain of Domino Borders
  *
  * 0 - Outer Border that should be displayed
  * 1 - Center Border (centerline of a domino) that should NOT be displayed
  */
  domain(Borders, 0, 1),

  /**
  * Domain of $A
  * As stated above, it's a throw-away variable, so it can have any value,
  * as long as it's NOT an already in use value
  */
  domain([A], 9, 9),

  /* --- ! SPECIFYING DOMAINS --- */


  /* --- SETTING CONSTRAINTS --- */

  /* get original Board (it's a matrix!) */
  board1(Board),

  /* get list of ALL available Dominos */
  dominos1(Dominos),

  /* get line length */
  lineLength1(LineLength),

  /* set the first constraint */
  neighbourshipConstraints(Board, Solution, 1-1, LineLength),

  /* set the second constraint */
  placementConstraints(Dominos, Solution, LineLength),

  /* --- ! SETTING CONSTRAINTS --- */


  /* --- LABELING --- */

  labeling([], Solution),

  /* --- PRINT --- */

  list_to_matrix(Solution, LineLength, Matrix),
  printBoard(Matrix, 0).
