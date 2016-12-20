/**
* Solver for Board #2 is implemented here
*/

/* board d = 6 */
board2([[1, 4, 3, 6, 6, 1, 0, 2, 2],
       [2, 0, 0, 0, 1, 1, 3, 1, 3],
       [2, 2, n, n, n, n, n, 0, 3],
       [3, 5, n, n, n, n, n, 6, 6],
       [3, 4, n, n, n, n, n, 6, 3],
       [5, 2, n, n, n, n, n, 2, 3],
       [5, 1, n, n, n, n, n, 6, 6],
       [1, 2, 0, 1, 0, 4, 4, 4, 5],
       [6, 4, 4, 4, 0, 5, 5, 5, 5]]).

/* original board width */
boardWidth2(9).

/* length of each line in $Solution */
lineLength2(LineLength):-
  boardWidth2(Width),
  LineLength is 2 * Width + 1.

/* set of available dominos (replace 0 with 7 and 1 with 8) */
dominos2([7-7, 7-8, 7-2, 7-3, 7-4, 7-5, 7-6, 8-8, 8-2, 8-3, 8-4, 8-5, 8-6, 2-2, 2-3, 2-4, 2-5, 2-6, 3-3, 3-4, 3-5, 3-6, 4-4, 4-5, 4-6, 5-5, 5-6, 6-6]).


/* --- SOLVER --- */

board2Solver:-

  /* --- DECLARING DOMAIN VARIABLES --- */

  /**
  * This a representation of the Board, along with Domino Borders.
  * This representation is a simple list (NOT a matrix!)
  *
  * Eyx - Eastern Border of the square (y, x) on the Board
  * Syx - Southern Border of the square (y, x) on the Board.
  * A - Throw-away variable. It's only here to make all "lines" the same length.
  * Consider it a representation of the corners. (notice that since this is NOT a matrix, there are in fact no "lines")
  * N - Null Square (it's supposed to be ignored when processing the list)
  * [2, 6] - These numbers represent the amount of dots (of the half-domino) on that square
  * 7 - Half-domino with 0 dots
  * 8 - Half-domino with 1 dot
  */
  Solution = [ A, S01, A, S02, A, S03, A, S04, A, S05, A, S06, A, S07, A, S08, A, S09, A,
              E10, 8, E11, 4, E12, 3, E13, 6, E14, 6, E15, 8, E16, 7, E17, 2, E18, 2, E19,
               A, S11, A, S12, A, S13, A, S14, A, S15, A, S16, A, S17, A, S18, A, S19, A,
              E20, 2, E21, 7, E22, 7, E23, 7, E24, 8, E25, 8, E26, 3, E27, 8, E28, 3, E29,
               A, S21, A, S22, A, S23, A, S24, A, S25, A, S26, A, S27, A, S28, A, S29, A,
              E30, 2, E31, 2, E32, N, E33, N, E34, N, E35, N, E36, N, E37, 7, E38, 3, E39,
               A, S31, A, S32, A, S33, A, S34, A, S35, A, S36, A, S37, A, S38, A, S39, A,
              E40, 3, E41, 5, E42, N, E43, N, E44, N, E45, N, E46, N, E47, 6, E48, 6, E49,
               A, S41, A, S42, A, S43, A, S44, A, S45, A, S46, A, S47, A, S48, A, S49, A,
              E50, 3, E51, 4, E52, N, E53, N, E54, N, E55, N, E56, N, E57, 6, E58, 3, E59,
               A, S51, A, S52, A, S53, A, S54, A, S55, A, S56, A, S57, A, S58, A, S59, A,
              E60, 5, E61, 2, E62, N, E63, N, E64, N, E65, N, E66, N, E67, 2, E68, 3, E69,
               A, S61, A, S62, A, S63, A, S64, A, S65, A, S66, A, S67, A, S68, A, S69, A,
              E70, 5, E71, 8, E72, N, E73, N, E74, N, E75, N, E76, N, E77, 6, E78, 6, E79,
               A, S71, A, S72, A, S73, A, S74, A, S75, A, S76, A, S77, A, S78, A, S79, A,
              E80, 8, E81, 2, E82, 7, E83, 8, E84, 7, E85, 4, E86, 4, E87, 4, E88, 5, E89,
               A, S81, A, S82, A, S83, A, S84, A, S85, A, S86, A, S87, A, S88, A, S89, A,
              E90, 6, E91, 4, E92, 4, E93, 4, E94, 7, E95, 5, E96, 5, E97, 5, E98, 5, E99,
               A, S91, A, S92, A, S93, A, S94, A, S95, A, S96, A, S97, A, S98, A, S99, A ],

  /**
  * A list containing ALL Eastern and Southern Borders declared above.
  *
  * While $Solution is used to set the constraints,
  * $Borders is used to set the domain of the variables.
  */
  Borders = [      S01, S02, S03, S04, S05, S06, S07, S08, S09,
              E10, E11, E12, E13, E14, E15, E16, E17, E18, E19,
                   S11, S12, S13, S14, S15, S16, S17, S18, S19,
              E20, E21, E22, E23, E24, E25, E26, E27, E28, E29,
                   S21, S22, S23, S24, S25, S26, S27, S28, S29,
              E30, E31, E32, E33, E34, E35, E36, E37, E38, E39,
                   S31, S32, S33, S34, S35, S36, S37, S38, S39,
              E40, E41, E42, E43, E44, E45, E46, E47, E48, E49,
                   S41, S42, S43, S44, S45, S46, S47, S48, S49,
              E50, E51, E52, E53, E54, E55, E56, E57, E58, E59,
                   S51, S52, S53, S54, S55, S56, S57, S58, S59,
              E60, E61, E62, E63, E64, E65, E66, E67, E68, E69,
                   S61, S62, S63, S64, S65, S66, S67, S68, S69,
              E70, E71, E72, E73, E74, E75, E76, E77, E78, E79,
                   S71, S72, S73, S74, S75, S76, S77, S78, S79,
              E80, E81, E82, E83, E84, E85, E86, E87, E88, E89,
                   S81, S82, S83, S84, S85, S86, S87, S88, S89,
              E90, E91, E92, E93, E94, E95, E96, E97, E98, E99,
                   S91, S92, S93, S94, S95, S96, S97, S98, S99 ],

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
  * as long as it's NOT a value already in use
  */
  domain([A], 9, 9),

  /**
  * Domain of $N
  * As stated above, it represents a null square, so it can have any value,
  * as long as it's NOT a value already in use
  */
  domain([N], 10, 10),

  /* --- ! SPECIFYING DOMAINS --- */


  /* --- SETTING CONSTRAINTS --- */

  /* get original Board (it's a matrix!) */
  board2(Board),

  /* get list of ALL available Dominos */
  dominos2(Dominos),

  /* get line length */
  lineLength2(LineLength),

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
