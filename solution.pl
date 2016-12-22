/**
* This files implements predicates related to $Solution,
* such as print, or getWidth
*/


/**
* Calculate length of each line of $Solution,
* based on Board's width
*
* @param BoardWidth Width of Board
* @param SolutionWidth Output - length of each line of $Solution
*/
solutionWidth(BoardWidth, SolutionWidth):-
  SolutionWidth is 2 * BoardWidth + 1.


/**
* Mapping of numbers in $Solution to char
*
* @param Number Number that is in the given square
* @param Char Mapped char
* @param Type Char type (sb -> char for southern border, sq -> char for square)
*/
toChar(0, '-', sb).
toChar(0, '|', sq).
toChar(1, ':', _).
toChar(9, ' ', _).
toChar(7, 0, _).
toChar(8, 1, _).
toChar(10, ' ', _).
toChar(X, X, _).


/**
* Prints lines corresponding to Shouthern Borders (even index, if Y starts at 0)
*
* @param Line Line to print
*/

/* stop condition */
printSouthBorder([]).

/* main case */
printSouthBorder([H|T]):-
  toChar(H, C, sb),
  write(C),
  printSouthBorder(T).


/**
* Prints lines corresponding to Squares (odd index, if Y starts at 0)
*
* @param Line Line to print
*/

/* stop condition */
printSquare([]).

/* main case */
printSquare([H|T]):-
  toChar(H, C, sq),
  write(C),
  printSquare(T).


/**
* Prints given line
*
* @param Line Line to print
* @param Y Index of line to print (start at 0)
*/

/* if even index */
printLine(Line, Y):-
  even(Y),
  printSouthBorder(Line),
  nl.

/* if odd index */
printLine(Line, Y):-
  \+ even(Y),
  printSquare(Line),
  nl.


/**
* Prints the Board (matrix!)
*
* @param Board Board to print
* @param Y Index of current line being iterated (starts at 0)
*/

/* stop condition */
printBoard([], _).

/* main case */
printBoard([Line|T], Y):-
  printLine(Line, Y),
  NewY is Y + 1,
  printBoard(T, NewY).
