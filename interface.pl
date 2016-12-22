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
  write('******* Domino puzzle: Main menu *******'),
  nl, nl,
  write('1 - solve first board'),
  nl,
  write('2 - solve second board'),
  nl,
  write('0 - exit SICStus'),
  nl, nl,
  getInt(I),
  nl,
  menu(I).
