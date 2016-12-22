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
findIndex(_, [], [], _).

/* main case - if elem was found */
findIndex(Elem, [Elem|T], [Index|Result], Index):-
  NewIndex is Index + 1,
  findIndex(Elem, T, Result, NewIndex).

/* main case - if elem was NOT found */
findIndex(Elem, [_|T], Result, Index):-
  NewIndex is Index + 1,
  findIndex(Elem, T, Result, NewIndex).
