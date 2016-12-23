/**
* All information related to Board 3 is stated here
* Predicates should have a 3 in the name,
* to indicate they are Board 3 predicates
*/

/* board d = 6 */
board3([[n, 2, 0, 4, 0, 1, n, n],
      [n, 5, 6, 3, 6, 6, 2, 1],
      [6, 5, 3, 4, 6, 5, 0, 1],
      [6, 5, 1, 0, 0, 5, 2, 1],
      [5, 4, 3, 2, 0, 1, 2, 4],
      [2, 3, 3, 4, 4, 1, 4, 4],
      [2, 6, 6, 2, 0, 0, 1, n],
      [n, n, 5, 5, 3, 3, 3, n]
]).

/* board width */
board3Width(8).

/* set of available dominos */
dominos3([0-0, 0-1, 0-2, 0-3, 0-4, 0-5, 0-6, 1-1, 1-2, 1-3, 1-4, 1-5, 1-6, 2-2, 2-3, 2-4, 2-5, 2-6, 3-3, 3-4, 3-5, 3-6, 4-4, 4-5, 4-6, 5-5, 5-6, 6-6]).

/**
* Retrieves info regarding this Board
*
* @param Board Original Board
* @param BoardWidth Length of each line of Board
* @param Dominos List of available dominos
*/
getBoard3Info(Board, BoardWidth, Dominos):-
  board3(Board),
  dominos3(Dominos),
  board3Width(BoardWidth).
