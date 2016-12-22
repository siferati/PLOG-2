/**
* All information related to Board 2 is stated here
* Predicates should have a 2 in the name,
* to indicate they are Board 2 predicates
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

/* board width */
board2Width(9).

/* set of available dominos */
dominos2([0-0, 0-1, 0-2, 0-3, 0-4, 0-5, 0-6, 1-1, 1-2, 1-3, 1-4, 1-5, 1-6, 2-2, 2-3, 2-4, 2-5, 2-6, 3-3, 3-4, 3-5, 3-6, 4-4, 4-5, 4-6, 5-5, 5-6, 6-6]).

/**
* Retrieves info regarding this Board
*
* @param Board Original Board
* @param BoardWidth Length of each line of Board
* @param Dominos List of available dominos
*/
getBoard2Info(Board, BoardWidth, Dominos):-
  board2(Board),
  dominos2(Dominos),
  board2Width(BoardWidth).
