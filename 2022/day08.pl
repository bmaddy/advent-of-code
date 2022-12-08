:- use_module(library(dcg/basics)).

setup :-
    portray_text(true),
    working_directory(_, "/home/bmaddy/src/bmaddy/adventofcode/2022").

test_input(`30373
25512
65332
33549
35390`).

tree([]), [pos(X1,0)] -->
    [pos(X0,_)],
    "\n",
    { succ(X0,X1) }.
tree([tree(pos(X,Y0), H)]), [pos(X,Y1)] -->
    [pos(X,Y0)],
    digit(C),
    { succ(Y0, Y1),
      number_codes(H, [C]) }.
%?- test_input(I), phrase(tree(Ts), [pos(0,0)|`1`], Rest).

trees([]) --> [_], eos.
trees(Ts) --> tree(T1), trees(T2), { append(T1, T2, Ts) }.
%?- test_input(I), phrase(trees(Ts), [pos(0,0)|``], Rest).
%?- test_input(I), phrase(trees(Ts), [pos(0,0)|`1`], Rest).
%?- test_input(I), phrase(trees(Ts), [pos(0,0)|I], Rest).
