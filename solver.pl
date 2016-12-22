/**
* This file implements the Solver
*/

/**
* Solves the Domino Puzzle:
*   Declare $Solution, $Borders (and $A)
*   Domain of $Borders and $A
*   Neighbourship Constraints
*   Placement Constraints
*   Labeling
*
* @param Board Puzzle to solve
* @param BoardWidth Length of each line of Board
* @param Dominos List of available dominos to solve the puzzle with
* @param SolutionMatrix Output - Solved puzzle in the form of a matrix
*/
solver(Board, BoardWidth, Dominos, SolutionMatrix):-

  /* --- DECLARING DOMAIN VARIABLES --- */
  translateDomino(Dominos, TranslatedDominos),                                  /* translate dominos */
  solutionWidth(BoardWidth, SolutionWidth),                                     /* get length of each line of $Solution */
  translateBoard(Board, TranslatedBoard, BoardWidth),                           /* translate board */
  addBorders(TranslatedBoard, Solution, Borders, As, SolutionWidth),            /* fully translate board into $Solution */
  /* --- ! DECLARING DOMAIN VARIABLES --- */

  /* --- SPECIFYING DOMAINS --- */
  domain(Borders, 0, 1),                                                        /* 0 -> Outer Border, 1 -> Centerline */
  domain(As, 9, 9),                                                             /* throw-away variable, can have any value */
  /* --- ! SPECIFYING DOMAINS --- */

  /* --- SETTING CONSTRAINTS --- */
  neighbourshipConstraints(Board, Solution, 1-1, SolutionWidth),                /* set first constraint */
  placementConstraints(TranslatedDominos, Solution, SolutionWidth),             /* set second constraint */
  /* --- ! SETTING CONSTRAINTS --- */

  /* --- LABELING --- */
  labeling([], Solution),

  /* --- RETURN SOLUTION AS A MATRIX --- */
  list_to_matrix(Solution, SolutionWidth, SolutionMatrix).
