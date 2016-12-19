:-use_module(library(clpfd)).


flatten_list([], []).

flatten_list( [HeadList| TailList], Result) :-
    flatten_list( TailList, NewTailList),
    !,
    append(HeadList, NewTailList, Result).

flatten_list( [ HeadList | Tail ], [ HeadList | OtherTail ]) :-
    flatten_list( Tail, OtherTail).


list_to_matrix([], _, []).
list_to_matrix(List, Size, [Row|Matrix]):-
  list_to_matrix_row(List, Size, Row, Tail),
  list_to_matrix(Tail, Size, Matrix).

list_to_matrix_row(Tail, 0, [], Tail).
list_to_matrix_row([Item|List], Size, [Item|Row], Tail):-
  NSize is Size-1,
  list_to_matrix_row(List, NSize, Row, Tail).



find(Y-X, List, Elem):-
  find(Y-X, List, Elem, 1).

find(Y-X, [List|_], Elem, Y):-
  element(X, List, Elem).

find(Y-X, [_|T], Elem, AccY):-
  AccY2 #= AccY + 1,
  find(Y-X, T, Elem, AccY2).


/**
* Find Index of multiple ocurrences of ELem in List
* @param Elem Element to find
* @param List List to search
* @param Result Where the indexes are saved
* @param Index starting index for counting
*/
findIndex(_, [], [], _):- !.
findIndex(Elem, [Elem|T], [Index|Result], Index):-
  NewIndex is Index + 1,
  findIndex(Elem, T, Result, NewIndex),
  !.
findIndex(Elem, [_|T], Result, Index):-
  NewIndex is Index + 1,
  findIndex(Elem, T, Result, NewIndex),
  !.

toCharH(0, '-').
toCharH(1, '').
toCharH(X, X).
toCharV(0, '|').
toCharV(1, '.').
toCharV(X, X).

printBoard([]).

printBoard([[]|OT]):-
  nl,
  printBoard(OT).

printBoard([[H|T]|OT]):-
  write(H),
  write(' '),
  printBoard([T|OT]).
