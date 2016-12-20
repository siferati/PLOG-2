/* --- INCLUDES --- */

:-use_module(library(clpfd)).
:- use_module(library(lists)).
:- ensure_loaded('utils.pl').

/* --- FIRST CONSTRAINT --- */

/**
* Iterate through every square on the Board
* and set the constraint for each and every single one
*
* @param Board Original Board (matrix!)
* @param Solution Solution (NOT a matrix!)
* @param Y-X Position of a square on the Board (starts at 1-1)
*/

/* stop condition */
neighbourshipConstraints([], _, _).

/* current line is finished, move to next line */
neighbourshipConstraints([[]|T], Solution, Y-_X):-
  NewY is Y + 1,
  neighbourshipConstraints(T, Solution, NewY-1).

/* main case - set constraint on current square and move to next square */
neighbourshipConstraints([[_|T]|OT], Solution, Y-X):-
  setNeighbourshipConstraints(Solution, Y-X),
  NewX is X + 1,
  neighbourshipConstraints([T|OT], Solution, Y-NewX).


/**
* For each square on the board, we state that exactly
* one of the four line segments bordering it will be a centerline,
* i.e. the sum of the corresponding variables is 1.
* For example, for the square (2, 4)
* the constraint is: S14 + E23 + S24 + E24 = 1.
*
* @param Solution Solution (NOT a matrix!)
* @param Y-X Position of the square on the Board
*/
setNeighbourshipConstraints(Solution, Y-X):-
  boardWidth(Width),
  /* nSquares + nEasternBorder + Ey0 */
  LineLength is 2 * Width + 1,
  /* ignore southern border line and eastern borders */
  SquareIndex is LineLength + 2 * ( (Y - 1) * LineLength ) + (2 * X),
  /* fetch all four border indexes */
  NorthIndex is SquareIndex - LineLength,
  WestIndex is SquareIndex - 1,
  SouthIndex is SquareIndex + LineLength,
  EastIndex is SquareIndex + 1,
  /* fetch all four borders of square (y, x) */
  element(NorthIndex, Solution, NorthBorder),
  element(WestIndex, Solution, WestBorder),
  element(SouthIndex, Solution, SouthBorder),
  element(EastIndex, Solution, EastBorder),
  /* set the constraint explained above */
  NorthBorder + WestBorder + SouthBorder + EastBorder #= 1.

/* --- ! FIRST CONSTRAINT --- */


/* --- SECOND CONSTRAINT --- */

/**
* Iterate through every available Domino
* and set the constraint for each and every single one
*
* @param Dominos List of available Dominos
* @param Solution Solution (NOT a matrix!)
*/

/* stop condition */
placementConstraints([], _).

/* main case - find each domino's possible positions and set the constraint */
placementConstraints([H1-H2|Dominos], Solution):-
  /* get all occurrences of the first half of this domino */
  findIndex(H1, Solution, H1Indexes, 1),
  /* get all occurrences of the second half of this domino */
  findIndex(H2, Solution, H2Indexes, 1),
  /* get possible positions for this domino (aka, H1 and H2 are next to each other) */
  match(H1Indexes, H2Indexes, H2Indexes, Matches),
  /* clean list of any duplicates */
  remove_dups(Matches, MatchesNoDups),
  /* set the constraint for this domino */
  setPlacementConstraints(MatchesNoDups, Solution),
  /* recursive call */
  placementConstraints(Dominos, Solution).


/**
* Iterate through Matches and fetch the centerline of each domino match
*
* @param Matches List of possible positions for a Domino
* @param Solution Solution (NOT a matrix!)
* @param Borders Ouput List with all borders
*/

/* stop condition */
getCenterLines([], _, []).

/* main case */
getCenterLines([H|T], Solution, [Border|Borders]):-
  getCenterLine(H, Solution, Border),
  getCenterLines(T, Solution, Borders).


/**
* Get the centerline of a given domino
*
* @param Index-Dir Index of the FIRST-HALF (western or northern) of a Domino
* @param Index-Dir Direction of the Domino (0 -> Horizontal, 1 -> Vertical)
* @param Solution Solution (NOT a matrix!)
* @param Border Output (domain variable)
*/

/* If Horizontal Domino (ie SquareIndex = Western-half index) */
getCenterLine(SquareIndex-0, Solution, EastBorder):-
  EastIndex is SquareIndex + 1,
  element(EastIndex, Solution, EastBorder).

/* If Vertical Domino (ie SquareIndex = Northern-half index) */
getCenterLine(SquareIndex-1, Solution, SouthBorder):-
  boardWidth(Width),
  /* nSquares + nEasternBorder + Ey0 */
  LineLength is 2 * Width + 1,
  SouthIndex is SquareIndex + LineLength,
  element(SouthIndex, Solution, SouthBorder).


/*
* For each domino, consider all its possible placements.
* We state that exactly one of these placements has to be selected,
* i.e. from amongst the line segments in the centre of these placements,
* exactly one will be a domino centerline.
* For example, the <0, 2> domino of Fig. 4 gives rise
* to the following constraint: E22 + S34 + E44 = 1.
*
* @param Matches List of possible positions for a given Domino
* @param Solution Solution (NOT a matrix!)
*/
setPlacementConstraints(Matches, Solution):-
  /* get the centerlines of each possible position */
  getCenterLines(Matches, Solution, Borders),
  /* set constraint - only one of the these centerlines can actually be true */
  sum(Borders, #=, 1).


/** check if they are a domino match (same piece) and return indexes
0 -> horizontal
1 -> vertical */

/**
* Get ALL possible positions for a Domino, given ALL occurrences of both domino-halfs
*
* @param H1Indexes List of all occurrences of the first domino-half
* @param H2Indexes List of all occurrences of the second domino-half
* @param Matches Output - List of all possible positions for this Domino (<H1, H2>)
*/

/* stop condition */
match([], _, _, []).

/* when the current elem from H1Indexes has been fully tested with ALL H2Indexes,
move to the next H1Indexes elem */
match([_|H1T], [], H2Indexes, Matches):-
  match(H1T, H2Indexes, H2Indexes, Matches).

/* H1|H2 */
match([H1H|H1T], [H2H|H2T], H2Indexes, [H1H-0|Matches]):-
  H1H is H2H - 2,
  match([H1H|H1T], H2T, H2Indexes, Matches).

/* H2|H1 */
match([H1H|H1T], [H2H|H2T], H2Indexes, [H2H-0|Matches]):-
  H1H is H2H + 2,
  match([H1H|H1T], H2T, H2Indexes, Matches).

/* H1
   -
   H2
*/
match([H1H|H1T], [H2H|H2T], H2Indexes, [H1H-1|Matches]):-
  boardWidth(Width),
  /* nSquares + nEasternBorder + Ey0 */
  LineLength is 2 * Width + 1,
  H1H is H2H - 2 * LineLength,
  match([H1H|H1T], H2T, H2Indexes, Matches).

/* H2
   --
   H1
*/
match([H1H|H1T], [H2H|H2T], H2Indexes, [H2H-1|Matches]):-
  boardWidth(Width),
  /* nSquares + nEasternBorder + Ey0 */
  LineLength is 2 * Width + 1,
  H1H is H2H + 2 * LineLength,
  match([H1H|H1T], H2T, H2Indexes, Matches).

/* when the current elem from H1Indexes has been tested with current elem from H2Indexes,
move to the next H2Indexes elem */
match([H1H|H1T], [_|H2T], H2Indexes, Matches):-
  match([H1H|H1T], H2T, H2Indexes, Matches).

/* --- ! SECOND CONSTRAINT --- */

/* --- EXTRA STUFF --- */

/* board d = 3 */
board([[1, 3, 0, 1, 2],
       [3, 2, 0, 1, 3],
       [3, 3, 0, 0, 1],
       [2, 2, 1, 2, 0]]).

/* original board width */
boardWidth(5).

/* set of available dominos */
dominos([7-7, 7-8, 7-2, 7-3, 8-8, 8-2, 8-3, 2-2, 2-3, 3-3]).

/* --- ! EXTRA STUFF --- */

/* --- MAIN --- */

domino:-

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
  Solution = [ A,  S01, A, S02, A, S03, A, S04, A, S05,  A,
              E10,  8, E11, 3, E12, 7, E13, 8, E14, 2,  E15,
               A,  S11, A, S12, A, S13, A, S14, A, S15,  A,
              E20,  3, E21, 2, E22, 7, E23, 8, E24, 3,  E25,
               A,  S21, A, S22, A, S23, A, S24, A, S25,  A,
              E30,  3, E31, 3, E32, 7, E33, 7, E34, 8,  E35,
               A,  S31, A, S32, A, S33, A, S34, A, S35,  A,
              E40,  2, E41, 2, E42, 8, E43, 2, E44, 7,  E45,
               A,  S41, A, S42, A, S43, A, S44, A, S45,  A   ],

  /**
  * A list containing ALL Eastern and Southern Borders declared above.
  *
  * While $Solution is used to set the constraints,
  * $Borders is used to set the domain of the variables.
  */
  Borders = [ S01, S02, S03, S04, S05,
              E10, E11, E12, E13, E14, E15,
              S11, S12, S13, S14, S15,
              E20, E21, E22, E23, E24, E25,
              S21, S22, S23, S24, S25,
              E30, E31, E32, E33, E34, E35,
              S31, S32, S33, S34, S35,
              E40, E41, E42, E43, E44, E45,
              S41, S42, S43, S44, S45       ],

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
  board(Board),

  /* set the first constraint */
  neighbourshipConstraints(Board, Solution, 1-1),

  /* get list of ALL available Dominos */
  dominos(Dominos),

  /* set the second constraint */
  placementConstraints(Dominos, Solution),

  /* --- ! SETTING CONSTRAINTS --- */


  /* --- LABELING --- */

  labeling([], Solution),

  /* --- PRINT --- */

  list_to_matrix(Solution, 11, Matrix),
  printBoard(Matrix, 0).
