:- use_module(library(dcg/basics)).
:- autoload(library(readutil), [ read_file_to_codes/3
                               ]).

:- autoload(library(aggregate), [ aggregate_all/3
                                ]).

:- use_module(library(clpfd)).

:- autoload(library(lists), [ append/3,
                              member/2
                            ]).

% portray_text(true).
% cd("/home/bmaddy/src/bmaddy/adventofcode/2022").

test_input(`30373
25512
65332
33549
35390`).

read_tree([]), [pos(X1,0)] -->
    [pos(X0,_)],
    "\n",
    { succ(X0,X1) }.
read_tree([tree(pos(X,Y0), H)]), [pos(X,Y1)] -->
    [pos(X,Y0)],
    digit(C),
    { succ(Y0, Y1),
      number_codes(H, [C]) }.
%?- test_input(I), phrase(tree(Ts), [pos(0,0)|`1`], Rest).

trees([]) --> [_], eos.
trees(Ts) --> read_tree(T1), trees(T2), { append(T1, T2, Ts) }.
%?- test_input(I), phrase(trees(Ts), [pos(0,0)|``], Rest).
%?- test_input(I), phrase(trees(Ts), [pos(0,0)|`1`], Rest).
%?- test_input(I), phrase(trees(Ts), [pos(0,0)|I], Rest).

:- dynamic tree/2.

reload_trees(Ts) :-
    retractall(tree(_, _)),
    forall(member(T, Ts),
           assertz(T)).
%?- test_input(I), phrase(trees(Ts), [pos(0,0)|I], Rest), reload_trees(Ts).

load_trees(I) :-
    phrase(trees(Ts), [pos(0,0)|I]),
    reload_trees(Ts).
%?- load_test.

tree_blocker_north(tree(pos(X,Y), H), tree(pos(BlockingX,Y), BlockingH)) :-
    tree(pos(BlockingX,Y), BlockingH),
    BlockingX #< X,
    H #=< BlockingH.
tree_blocker_east(tree(pos(X,Y), H), tree(pos(X,BlockingY), BlockingH)) :-
    tree(pos(X,BlockingY), BlockingH),
    Y #< BlockingY,
    H #=< BlockingH.
tree_blocker_south(tree(pos(X,Y), H), tree(pos(BlockingX,Y), BlockingH)) :-
    tree(pos(BlockingX,Y), BlockingH),
    X #< BlockingX,
    H #=< BlockingH.
tree_blocker_west(tree(pos(X,Y), H), tree(pos(X,BlockingY), BlockingH)) :-
    tree(pos(X,BlockingY), BlockingH),
    BlockingY #< Y,
    H #=< BlockingH.
%?- tree_blocker_north(tree(pos(1,0), 1), B).
%?- tree_blocker_east(tree(pos(0,3), 1), B).
%?- tree_blocker_south(tree(pos(3,0), 1), B).
%?- tree_blocker_west(tree(pos(0,1), 1), B).

tree_blockers(T, [BN,BE,BS,BW]) :-
    tree_blocker_north(T, BN),
    tree_blocker_east(T, BE),
    tree_blocker_south(T, BS),
    tree_blocker_west(T, BW), !.
%?- tree(P,H), tree_blockers(tree(P,H), Bs).

test1(Ans) :-
    test_input(I),
    load_trees(I),
    aggregate_all(count,
                  ( tree(P,H),
                    \+ tree_blockers(tree(P,H), _)
                  ),
                  Ans).
%?- time(test1(Ans)).
% Ans = 21 ;

solve1(Ans) :-
    read_file_to_codes("input08.txt", I, []),
    load_trees(I),
    aggregate_all(count,
                  ( tree(P,H),
                    \+ tree_blockers(tree(P,H), _)
                  ),
                  Ans).
%?- solve1(Ans).
% Ans = 1825

up(pos(X0,Y), pos(X1,Y)) :- X1 #= X0 - 1.
down(pos(X0,Y), pos(X1,Y)) :- X1 #= X0 + 1.
left(pos(X,Y0), pos(X,Y1)) :- Y1 #= Y0 - 1.
right(pos(X,Y0), pos(X,Y1)) :- Y1 #= Y0 + 1.

visible_trees_dir(tree(P, H), N, Dir) :-
    call(Dir, P, P1),
    ( tree(P1, H1)
    -> ( H1 #< H,
         visible_trees_dir(tree(P1, H), N1, Dir)
       -> succ(N1, N)
       ;
         N #= 1 )
    ;
      N #= 0 ).
%?- member(D, [up,down,left,right]), visible_trees_dir(tree(pos(0,0), 1), N, D).
%?- member(D, [up,down,left,right]), visible_trees_dir(tree(pos(1,2), 5), N, D).
%?- member(D, [up,down,left,right]), visible_trees_dir(tree(pos(3,2), 5), N, D).

senic_score(T, Score) :-
    visible_trees_dir(T, U, up),
    visible_trees_dir(T, D, down),
    visible_trees_dir(T, L, left),
    visible_trees_dir(T, R, right),
    Score #= U*D*L*R.
%?- senic_score(tree(pos(1,2), 5), S).
%?- senic_score(tree(pos(3,2), 5), S).

test2(Ans) :-
    test_input(I),
    load_trees(I),
    aggregate_all(max(S),
                  ( tree(P, H),
                    senic_score(tree(P, H), S)),
                 Ans).
%?- test2(Ans).
% Ans = 8

solve2(Ans) :-
    read_file_to_codes("input08.txt", I, []),
    load_trees(I),
    aggregate_all(max(S),
                  ( tree(P, H),
                    senic_score(tree(P, H), S)),
                 Ans).
%?- solve2(Ans).
% Ans = 235200
