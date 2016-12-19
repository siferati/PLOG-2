:-use_module(library(clpfd)).
:- use_module(library(lists)).
:- ensure_loaded('utils.pl').

/*
For each square on the board we state that exactly
one of the four line segments bordering it will be a centerline,
i.e. the sum of the corresponding variables is 1.
For example, for the square (2, 4)
the constraint is: S14 + E23 + S24 + E24 = 1.
*/

/**
* Sets the above explained constraint
* @param Board Original Board with the domino dots
* @param Solution Original Board + Boarders
* @param Y-X Position on Board (starts at 1)
*/
iterator([], _, _).

iterator([[]|T], Solution, Y-_X):-
  NewY is Y + 1,
  iterator(T, Solution, NewY-1).

iterator([[_|T]|OT], Solution, Y-X):-
  neighbourshipConstraints(Solution, Y-X),
  NewX is X + 1,
  iterator([T|OT], Solution, Y-NewX).

neighbourshipConstraints(Solution, Y-X):-
  % 5 = board width
  LineLength is 2 * 5 + 1,
  SquareIndex is LineLength + 2 * ( (Y - 1) * LineLength ) + (2 * X),
  NorthIndex is SquareIndex - LineLength,
  WestIndex is SquareIndex - 1,
  SouthIndex is SquareIndex + LineLength,
  EastIndex is SquareIndex + 1,
  element(NorthIndex, Solution, NorthBorder),
  element(WestIndex, Solution, WestBorder),
  element(SouthIndex, Solution, SouthBorder),
  element(EastIndex, Solution, EastBorder),
  NorthBorder + WestBorder + SouthBorder + EastBorder #= 1.

/*
For each domino, consider all its possible placements.
We state that exactly one of these placements has to be selected,
i.e. from amongst the line segments in the centre of these placements,
exactly one will be a domino centerline.
For example, the 0-2 domino of Fig. 4 gives rise
to the following constraint: E22 + S34 + E44 = 1.
*/
iterator2([], _).

iterator2([H1-H2|Dominos], Solution):-
  findIndex(H1, Solution, H1Indexes, 1),
  findIndex(H2, Solution, H2Indexes, 1),
  match(H1Indexes, H2Indexes, H2Indexes, Matches),
  remove_dups(Matches, MatchesNoDups),
  placementConstraints(MatchesNoDups, Solution),
  iterator2(Dominos, Solution).

getBorders([], _, []).

getBorders([H|T], Solution, [Border|Borders]):-
  getBorder(H, Solution, Border),
  getBorders(T, Solution, Borders).

getBorder(SquareIndex-0, Solution, EastBorder):-
  EastIndex is SquareIndex + 1,
  element(EastIndex, Solution, EastBorder).

getBorder(SquareIndex-1, Solution, SouthBorder):-
  % 5 = board width
  LineLength is 2 * 5 + 1,
  SouthIndex is SquareIndex + LineLength,
  element(SouthIndex, Solution, SouthBorder).

placementConstraints(Matches, Solution):-
  getBorders(Matches, Solution, Borders),
  sum(Borders, #=, 1).


/** check if they are a domino match (same piece) and return indexes
0 -> horizontal
1 -> vertical */

match([], _, _, []).

match([_|H1T], [], H2Indexes, Result):-
  match(H1T, H2Indexes, H2Indexes, Result).

/* H1|H2 */
match([H1H|H1T], [H2H|H2T], H2Indexes, [H1H-0|Result]):-
  H1H is H2H - 2,
  match([H1H|H1T], H2T, H2Indexes, Result).

/* H2|H1 */
match([H1H|H1T], [H2H|H2T], H2Indexes, [H2H-0|Result]):-
  H1H is H2H + 2,
  match([H1H|H1T], H2T, H2Indexes, Result).

/* H1
   -
   H2
*/
match([H1H|H1T], [H2H|H2T], H2Indexes, [H1H-1|Result]):-
  % 5 = board width
  LineLength is 2 * 5 + 1,
  H1H is H2H - 2 * LineLength,
  match([H1H|H1T], H2T, H2Indexes, Result).

/* H2
   --
   H1
*/
match([H1H|H1T], [H2H|H2T], H2Indexes, [H2H-1|Result]):-
  % 5 = board width
  LineLength is 2 * 5 + 1,
  H1H is H2H + 2 * LineLength,
  match([H1H|H1T], H2T, H2Indexes, Result).

match([H1H|H1T], [_|H2T], H2Indexes, Result):-
  match([H1H|H1T], H2T, H2Indexes, Result).



/* board d = 3 */
board([[1, 3, 0, 1, 2],
       [3, 2, 0, 1, 3],
       [3, 3, 0, 0, 1],
       [2, 2, 1, 2, 0]]).

dominos([7-7, 7-8, 7-2, 7-3, 8-8, 8-2, 8-3, 2-2, 2-3, 3-3]).

board_flat([1, 3, 0, 1, 2,
            3, 2, 0, 1, 3,
            3, 3, 0, 0, 1,
            2, 2, 1, 2, 0]).

/* solution d = 3
[[n, w, e, n, n],
 [s, w, e, s, s],
 [w, e, w, e, n],
 [w, e, w, e, s]] */

domino:-

  % VARIABLES

  % 0 dots -> 7
  % 1 dot -> 8
  % A -> means nothing, just to make a square list
  Solution = [ A,  S01, A, S02, A, S03, A, S04, A, S05,  A,
              E10,  8, E11, 3, E12, 7, E13, 8, E14, 2,  E15,
               A,  S11, A, S12, A, S13, A, S14, A, S15,  A,
              E20,  3, E21, 2, E22, 7, E23, 8, E24, 3,  E25,
               A,  S21, A, S22, A, S23, A, S24, A, S25,  A,
              E30,  3, E31, 3, E32, 7, E33, 7, E34, 8,  E35,
               A,  S31, A, S32, A, S33, A, S34, A, S35,  A,
              E40,  2, E41, 2, E42, 8, E43, 2, E44, 7,  E45,
               A,  S41, A, S42, A, S43, A, S44, A, S45,  A],

  % For each domino i,j ∈ Dd, we set up a domino variable DVij specifying the position of this domino on the board.
  Borders = [S01, S02, S03, S04, S05,
             E10, E11, E12, E13, E14, E15,
             S11, S12, S13, S14, S15,
             E20, E21, E22, E23, E24, E25,
             S21, S22, S23, S24, S25,
             E30, E31, E32, E33, E34, E35,
             S31, S32, S33, S34, S35,
             E40, E41, E42, E43, E44, E45,
             S41, S42, S43, S44, S45],


  % DOMAINS

  % 1 means it's a center line of a domino, 0 otherwise
  domain(Borders, 0, 1),

  % RESTRICTIONS
  A #= 9,

  board(Board),
  iterator(Board, Solution, 1-1),

  dominos(Dominos),

  iterator2(Dominos, Solution),

  % LABELING
  labeling([], Solution),

  % PRINT
  list_to_matrix(Solution, 11, Matrix),
  printBoard(Matrix).
