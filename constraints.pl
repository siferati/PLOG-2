/**
* This file implements the two constraints needed to solve the puzzle:
* The Neighbourship Constraint
* The Placement Constraint
*/

/* --- FIRST CONSTRAINT --- */

/**
* Iterate through every square on the Board
* and set the constraint for each and every single one
*
* @param Board Original Board (matrix!)
* @param Solution Solution (NOT a matrix!)
* @param Y-X Position of a square on the Board (starts at 1-1)
* @param LineLength Length of each line of Solution
*/

/* stop condition */
neighbourshipConstraints([], _, _, _).

/* current line is finished, move to next line */
neighbourshipConstraints([[]|T], Solution, Y-_X, LineLength):-
  NewY is Y + 1,
  neighbourshipConstraints(T, Solution, NewY-1, LineLength).

/* if current square is null, then move on */
neighbourshipConstraints([[n|T]|OT], Solution, Y-X, LineLength):-
  NewX is X + 1,
  neighbourshipConstraints([T|OT], Solution, Y-NewX, LineLength).

/* main case - set constraint on current square and move to next square */
neighbourshipConstraints([[_|T]|OT], Solution, Y-X, LineLength):-
  setNeighbourshipConstraints(Solution, Y-X, LineLength),
  NewX is X + 1,
  neighbourshipConstraints([T|OT], Solution, Y-NewX, LineLength).


/**
* For each square on the board, we state that exactly
* one of the four line segments bordering it will be a centerline,
* i.e. the sum of the corresponding variables is 1.
* For example, for the square (2, 4)
* the constraint is: S14 + E23 + S24 + E24 = 1.
*
* @param Solution Solution (NOT a matrix!)
* @param Y-X Position of the square on the Board
* @param LineLength Length of each line of Solution
*/
setNeighbourshipConstraints(Solution, Y-X, LineLength):-
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
* @param LineLength Length of each line of Solution
*/

/* stop condition */
placementConstraints([], _, _).

/* main case - find each domino's possible positions and set the constraint */
placementConstraints([H1-H2|Dominos], Solution, LineLength):-
  /* get all occurrences of the first half of this domino */
  findIndex(H1, Solution, H1Indexes, 1),
  /* get all occurrences of the second half of this domino */
  findIndex(H2, Solution, H2Indexes, 1),
  /* get possible positions for this domino (aka, H1 and H2 are next to each other) */
  match(H1Indexes, H2Indexes, H2Indexes, Matches, LineLength),
  /* clean list of any duplicates */
  remove_dups(Matches, MatchesNoDups),
  /* set the constraint for this domino */
  setPlacementConstraints(MatchesNoDups, Solution, LineLength),
  /* recursive call */
  placementConstraints(Dominos, Solution, LineLength).


/**
* Get ALL possible positions for a Domino, given ALL occurrences of both domino-halfs
*
* @param H1Indexes List of all occurrences of the first domino-half
* @param H2Indexes List of all occurrences of the second domino-half
* @param Matches Output - List of all possible positions for this Domino (<H1, H2>)
* @param LineLength Length of each line of Solution
*/

/* stop condition */
match([], _, _, [], _).

/* when the current elem from H1Indexes has been fully tested with ALL H2Indexes,
move to the next H1Indexes elem */
match([_|H1T], [], H2Indexes, Matches, LineLength):-
  match(H1T, H2Indexes, H2Indexes, Matches, LineLength).

/* H1|H2 */
match([H1H|H1T], [H2H|H2T], H2Indexes, [H1H-0|Matches], LineLength):-
  H1H is H2H - 2,
  match([H1H|H1T], H2T, H2Indexes, Matches, LineLength).

/* H2|H1 */
match([H1H|H1T], [H2H|H2T], H2Indexes, [H2H-0|Matches], LineLength):-
  H1H is H2H + 2,
  match([H1H|H1T], H2T, H2Indexes, Matches, LineLength).

/* H1
   -
   H2
*/
match([H1H|H1T], [H2H|H2T], H2Indexes, [H1H-1|Matches], LineLength):-
  H1H is H2H - 2 * LineLength,
  match([H1H|H1T], H2T, H2Indexes, Matches, LineLength).

/* H2
   --
   H1
*/
match([H1H|H1T], [H2H|H2T], H2Indexes, [H2H-1|Matches], LineLength):-
  H1H is H2H + 2 * LineLength,
  match([H1H|H1T], H2T, H2Indexes, Matches, LineLength).

/* when the current elem from H1Indexes has been tested with current elem from H2Indexes,
move to the next H2Indexes elem */
match([H1H|H1T], [_|H2T], H2Indexes, Matches, LineLength):-
  match([H1H|H1T], H2T, H2Indexes, Matches, LineLength).


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
* @param LineLength Length of each line of Solution
*/
setPlacementConstraints(Matches, Solution, LineLength):-
  /* get the centerlines of each possible position */
  getCenterLines(Matches, Solution, Borders, LineLength),
  /* set constraint - only one of the these centerlines can actually be true */
  sum(Borders, #=, 1).


/**
* Iterate through Matches and fetch the centerline of each domino match
*
* @param Matches List of possible positions for a Domino
* @param Solution Solution (NOT a matrix!)
* @param Borders Ouput List with all borders
* @param LineLength Length of each line of Solution
*/

/* stop condition */
getCenterLines([], _, [], _).

/* main case */
getCenterLines([H|T], Solution, [Border|Borders], LineLength):-
  getCenterLine(H, Solution, Border, LineLength),
  getCenterLines(T, Solution, Borders, LineLength).


/**
* Get the centerline of a given domino
*
* @param Index-Dir Index of the FIRST-HALF (western or northern) of a Domino
* @param Index-Dir Direction of the Domino (0 -> Horizontal, 1 -> Vertical)
* @param Solution Solution (NOT a matrix!)
* @param Border Output (domain variable)
* @param LineLength Length of each line of Solution
*/

/* If Horizontal Domino (ie SquareIndex = Western-half index) */
getCenterLine(SquareIndex-0, Solution, EastBorder, _):-
  EastIndex is SquareIndex + 1,
  element(EastIndex, Solution, EastBorder).

/* If Vertical Domino (ie SquareIndex = Northern-half index) */
getCenterLine(SquareIndex-1, Solution, SouthBorder, LineLength):-
  SouthIndex is SquareIndex + LineLength,
  element(SouthIndex, Solution, SouthBorder).

/* --- ! SECOND CONSTRAINT --- */
