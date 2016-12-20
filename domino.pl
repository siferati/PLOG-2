/**
* The main loop is implemented here.
* All dependencies are also declared here,
* so other files don't include them as well.
*/

/* --- INCLUDES --- */

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- ensure_loaded('board1Solver.pl').
:- ensure_loaded('board2Solver.pl').
:- ensure_loaded('constraints.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('utils.pl').


/* --- MAIN --- */

domino:-
  interface,
  domino.
