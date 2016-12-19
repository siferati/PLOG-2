:-use_module(library(clpfd)).
:- ensure_loaded('utils.pl').

/* if top is north, bottom needs to be south*/
neighbourshipConstraints(0, CV2, CV3):-
  CV2 #= 1.

/* if left is west, right needs to be east */
neighbourshipConstraints(4, CV2, CV3):-
  CV3 #= 3.




/**
* The compass model
*/


/* translation for compass variables
  0 -> n
  1 -> s
  3 -> e
  4 -> w
*/


/* board d = 3 */
board([[1, 3, 0, 1, 2],
       [3, 2, 0, 1, 3],
       [3, 3, 0, 0, 1],
       [2, 2, 1, 2, 0]]).

/* solution d = 3
[[n, w, e, n, n],
 [s, w, e, s, s],
 [w, e, w, e, n],
 [w, e, w, e, s]] */


/* Available Dominos d = 3 */
D([0-0, 0-1, 0-2, 0-3, 1-1, 1-2, 1-3, 2-2, 2-3, 3-3]).


domino:-

  % variables
  % CVyx, 1 ≤ y ≤ maxy, 1 ≤ x ≤ maxx, (where maxy and maxx are the number of rows and columns of the board)
  Solution = [[CV11, CV12, CV13, CV14, CV15],
              [CV21, CV22, CV23, CV24, CV25],
              [CV31, CV32, CV33, CV34, CV35],
              [CV41, CV42, CV43, CV44, CV45],

 % triplet (row, column, dir)
 DV = [R1-C1-D1, R2-C2-D2, R3-C3-D3, R4-C4-D4, R5-C5-D5, R6-C6-D6, R7-C7-D7, R8-C8-D8, R9-C9-D9, R10-C10-D10, R11-C11-D11, R12-C12-D12, R13-C13-D13, R14-C14-D14, R15-C15-D15],

  % domain (see translation at the top)
  domain(Solution, 0, 4),
  domain(Y, 1, 5),
  domain(X, 1, 6),

  % restriction
  find(Y-X, Solution, CV),
  Y2 #= Y+1,
  X2 #= X+1,
  find(Y2-X, Solution, CV2),
  find(Y-X2, Solution, CV3),
  neighbourshipConstraints(CV, CV2, CV3),

  %labeling
  labeling([], Solution).
