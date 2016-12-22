/**
* All information related to Board 1 is stated here
* Predicates should have a 1 in their name,
* to indicate they are Board 1 predicates
*/

/* board d = 3 */
board1([[1, 3, 0, 1, 2],
       [3, 2, 0, 1, 3],
       [3, 3, 0, 0, 1],
       [2, 2, 1, 2, 0]]).

/* board width */
board1Width(5).

/* set of available dominos */
dominos1([0-0, 0-1, 0-2, 0-3, 1-1, 1-2, 1-3, 2-2, 2-3, 3-3]).


/**
* Retrieves info regarding this Board
*
* @param Board Original Board
* @param BoardWidth Length of each line of Board
* @param Dominos List of available dominos
*/
getBoard1Info(Board, BoardWidth, Dominos):-
  board1(Board),
  dominos1(Dominos),
  board1Width(BoardWidth).
