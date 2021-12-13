:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

:- table map_well_neighbors/4.

test_input(`2199943210
3987894921
9856789892
8767896789
9899965678`).

height(H) --> digit(D), { number_codes(H, [D]) }.
row([H]) --> height(H), eol.
row([H|Hs]) --> height(H), row(Hs).
%?- phrase(row(R), `123`).

heightmap([]) --> eos.
heightmap([R|Rs]) --> row(R), heightmap(Rs).
%?- phrase(heightmap(M), `123\n123`, Rest).

adjacent_pos(pos(X0,Y), pos(X1,Y)) :- succ(X0, X1).
adjacent_pos(pos(X0,Y), pos(X1,Y)) :- succ(X1, X0).
adjacent_pos(pos(X,Y0), pos(X,Y1)) :- succ(Y0, Y1).
adjacent_pos(pos(X,Y0), pos(X,Y1)) :- succ(Y1, Y0).
%?- adjacent_pos(pos(1,1), A).

get_value(pos(X,Y), M, V) :-
    nth0(Y, M, Row),
    nth0(X, Row, V).
%?- get_value(pos(2,2), [[1,2,3], [4,5,6], [7,8,9]], A).
%?- get_value(pos(9,9), [[1,2,3], [4,5,6], [7,8,9]], A).
%?- get_value(P, [[1,2,3], [4,5,6], [7,8,9]], _).

adjacent_value(M, Pos, V) :-
    get_value(Pos1, M, V),
    adjacent_pos(Pos, Pos1).
%?- adjacent_value([[1,2,3], [4,5,6], [7,8,9]], pos(2,2), V).
%?- adjacent_value([[1,2,3], [4,5,6], [7,8,9]], pos(1,1), V).
%?- adjacent_value([[1,2,3], [4,5,6], [7,8,9]], P, 5).

map_well_neighbors(M, well(Well, Height, Min), Ns, Risk) :-
    get_value(Well, M, Height),
    % all heights adjacent (max of 4) to this well
    bagof(Adj, adjacent_value(M, Well, Adj), Ns),
    min_member(Min, Ns),
    Height #< Min,
    succ(Height, Risk).
%?- map_well_neighbors([[1,2,3], [4,5,6], [7,8,9]], Well, Ns).
%?- test_input(Input), phrase(heightmap(M), Input), map_well_neighbors(M, Well, Ns, R).

solve1(M, Risk) :-
    findall(R, map_well_neighbors(M, _, _, R), Rs),
    foldl(plus, Rs, 0, Risk).

test1(Answer) :-
    test_input(Input),
    phrase(heightmap(M), Input),
    solve1(M, Answer).
%?- test1(15).

part1(Answer) :-
    phrase_from_file(heightmap(M), "input09.txt"),
    solve1(M, Answer).
%?- time(part1(Answer)).
%@ % 110,748 inferences, 0.013 CPU in 0.015 seconds (88% CPU, 8532861 Lips)
%@ Answer = 504 ;
%@ % 1,123 inferences, 0.000 CPU in 0.000 seconds (91% CPU, 4352713 Lips)
%@ false.
