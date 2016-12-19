:-use_module(library(clpfd)).

find(Y-X, List, Elem):-
  find(Y-X, List, Elem, 1).

find(Y-X, [List|_], Elem, Y):-
  element(X, List, Elem).

find(Y-X, [_|T], Elem, AccY):-
  AccY2 #= AccY + 1,
  find(Y-X, T, Elem, AccY2).
