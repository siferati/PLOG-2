/**
* All information related to Board 4 is stated here
* Predicates should have a 4 in the name,
* to indicate they are Board 4 predicates
*/

/* board d = 8 */
board4([[0, 1, 4, 3, 2, 0, 6, 5, 7, 1, 2, 4, 7, 1, 3],
        [0, 0, 6, 4, 2, n, n, n, n, n, 4, 4, 8, 7, 4],
        [1, 1, 6, 1, 8, n, n, n, n, n, 0, 6, 7, 6, 6],
        [5, 7, 0, 8, 3, n, n, n, n, n, 3, 1, 2, 2, 7],
        [4, 3, 6, 0, 3, n, n, n, n, n, 3, 1, 1, 5, 7],
        [4, 6, 6, 2, 3, n, n, n, n, n, 5, 8, 8, 3, 7],
        [4, 5, 5, 2, 7, n, n, n, n, n, 5, 0, 8, 6, 8],
        [0, 0, 8, 2, 5, 4, 2, 1, 2, 3, 7, 5, 5, 8, 8]
    ]).

/* board width */
board4Width(15).

/* set of available dominos */
dominos4([0-0, 0-1, 0-2, 0-3, 0-4, 0-5, 0-6, 0-7, 0-8, 1-1, 1-2, 1-3, 1-4, 1-5, 1-6, 1-7, 1-8, 2-2, 2-3, 2-4, 2-5, 2-6, 2-7, 2-8, 3-3, 3-4, 3-5, 3-6, 3-7, 3-8, 4-4, 4-5, 4-6, 4-7, 4-8, 5-5, 5-6, 5-7, 5-8, 6-6, 6-7, 6-8, 7-7, 7-8, 8-8]).

/**
* Retrieves info regarding this Board
*
* @param Board Original Board
* @param BoardWidth Length of each line of Board
* @param Dominos List of available dominos
*/
getBoard4Info(Board, BoardWidth, Dominos):-
  board4(Board),
  dominos4(Dominos),
  board4Width(BoardWidth).
