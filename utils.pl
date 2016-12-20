/**
* Utilities
*/

/**
* Checks if number is even
* @param N Number to check
*/
even(N):-
  0 is N mod 2.


/**
* Interface for get_char, when it ends with \n
* @param Input Input
*/
getChar(Input):-
  get_char(Input),      /* read input */
  get_char(_).          /* ignore \n */

/**
* Interface for get_code, when it ends with \n
* @param Input Input
*/
getCode(Input):-
  get_code(Input),      /* read input */
  get_code(_).          /* ignore \n */

/**
* Read int from temrinal
* @param Input Int to read
*/
getInt(Input):-
  get_code(Char),         /* read asci code */
  get_code(_),
  Input is Char - 48.     /* input = char - '0' */


/**
* Turn a list into a matrix
* (this was taken from Stack Overflow)
*
* @param List List to turn into matrix
* @param Length Length of each line in the returned matrix
* @param Matrix Output
*/

/* stop condition */
list_to_matrix([], _, []).

/* main case */
list_to_matrix(List, Length, [Row|Matrix]):-
  list_to_matrix_row(List, Length, Row, Tail),
  list_to_matrix(Tail, Length, Matrix).


/**
* Aux predicate to list_to_matrix
*/

/* stop condition */
list_to_matrix_row(Tail, 0, [], Tail).

/* main case */
list_to_matrix_row([Item|List], Length, [Item|Row], Tail):-
  NewLength is Length - 1 ,
  list_to_matrix_row(List, NewLength, Row, Tail).


/**
* Find Index of multiple ocurrences of ELem in List
*
* @param Elem Element to find
* @param List List to search
* @param Result Where the indexes are saved
* @param Index starting index for counting
*/

/* stop condition */
findIndex(_, [], [], _):- !.

/* main case - if elem was found */
findIndex(Elem, [Elem|T], [Index|Result], Index):-
  NewIndex is Index + 1,
  findIndex(Elem, T, Result, NewIndex),
  !.

/* main case - if elem was NOT found */
findIndex(Elem, [_|T], Result, Index):-
  NewIndex is Index + 1,
  findIndex(Elem, T, Result, NewIndex),
  !.


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
