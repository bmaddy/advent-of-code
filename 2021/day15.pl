:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

%% :- table map_well_neighbors/4.

test_input(`1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581`).

single_digit(H) --> digit(D), { number_codes(H, [D]) }.
row([H]) --> single_digit(H), eol.
row([H|Hs]) --> single_digit(H), row(Hs).
%?- phrase(row(R), `123`).

digit_map([]) --> eos.
digit_map([R|Rs]) --> row(R), digit_map(Rs).
%?- phrase(digit_map(M), `123\n123`, Rest).

adjacent_pos(pos(X0,Y), pos(X1,Y)) :- succ(X0, X1).
adjacent_pos(pos(X0,Y), pos(X1,Y)) :- succ(X1, X0).
adjacent_pos(pos(X,Y0), pos(X,Y1)) :- succ(Y0, Y1).
adjacent_pos(pos(X,Y0), pos(X,Y1)) :- succ(Y1, Y0).
%?- adjacent_pos(pos(0,0), A).

val_at(V, M, pos(X,Y)) :-
    nth0(Y, M, Row),
    nth0(X, Row, V).
%?- val_at(V, [[1,2,3], [4,5,6], [7,8,9]], pos(2,2)).
%?- val_at(V, [[1,2,3], [4,5,6], [7,8,9]], pos(9,9)).
%?- val_at(_, [[1,2,3], [4,5,6], [7,8,9]], P).

min_cost_to(M, S, C, E) :-
    adjacent_pos(S, E),
    val_at(C, M, E).
min_cost_to(Map, S, Cost, E) :-
    min_cost_to(Map, S, C1, M),
    min_cost_to(Map, M, C2, E),
    Cost #= C1 + C2.
:- table min_cost_to/4.
%?- min_cost_to([[1,2,3], [4,5,6], [7,8,9]], pos(0,0), 2, pos(0,1)).
