:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

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

%% map_contains([R|Rs], pos(X,Y)) :-
%%     length(R, W),
%%     length([R|Rs], H),
%%     0 #< W,
%%     0 #< H,
%%     X #< W,
%%     Y #< H.
%?- map_contains([[1,1], [1,1]], pos(X,Y)).

%% adjacent_map_pos(M, P0, P1) :-
%%     adjacent_pos(P0, P1),
%%     map_contains(M, P1).
%?- adjacent_map_pos([[1,1,1], [1,1,1], [1,1,1]], pos(1,1), A).

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

%% Working here
%% map_well_neighbors(M, Well, Ns, Risk) :-
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
%?- part1(Answer).
%@ Answer = 504 ;
%@ false.
