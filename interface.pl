/**
* This file implements the project's interface
*/

/* --- MENU --- */

menu(0):-
  halt.

menu(1):-
  getBoard1Info(Board, BoardWidth, Dominos),
  solver(Board, BoardWidth, Dominos, Solution),
  printBoard(Solution, 0).

menu(2):-
  getBoard2Info(Board, BoardWidth, Dominos),
  solver(Board, BoardWidth, Dominos, Solution),
  printBoard(Solution, 0).

/* if wrong input */
menu(_):-
  write('Wrong input, please repeat!').


/* --- INTERFACE --- */

interface:-
  nl,
  write('Press 1 to play board1, 2 to play board2, 0 to exit.'),
  nl,
  getInt(I),
  menu(I).
