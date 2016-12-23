/**
* Implementation was heavily inspired from
* 'Teaching Constraints through Logic Puzzles' by Péter Szeredi,
* Dept. of Computer Science and Information Theory,
* Budapest University of Technology and Economics
* (http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.97.9715&rep=rep1&type=pdf)
*
* The main loop is implemented here.
* All dependencies are also declared here,
* so other files don't include any.
*
* This implementation follows 'the border model',
* which is explained in the article mentioned above.
*/

/* --- INCLUDES --- */

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- ensure_loaded('boards/board1.pl').
:- ensure_loaded('boards/board2.pl').
:- ensure_loaded('boards/board3.pl').
:- ensure_loaded('boards/board4.pl').
:- ensure_loaded('board-translations.pl').
:- ensure_loaded('constraints.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('solution.pl').
:- ensure_loaded('solver.pl').
:- ensure_loaded('utils.pl').


/* --- MAIN --- */

main:-
  interface,
  main.
