:- use_module(library(dcg/basics)).
:- autoload(library(readutil), [ read_file_to_codes/3
                               ]).

test_input(`2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5
`).

point_3d(cube(pos(X,Y,Z))) --> number(X), ",", number(Y), ",", number(Z), eol.
%?- test_input(I), phrase(point_3d(P), I, Rest).
cubes([]) --> eos.
cubes([V|Vs]) --> point_3d(V), cubes(Vs).
%?- test_input(I), phrase(cubes(V), I, Rest).
%?- read_file_to_codes("input18.txt", I, []), phrase(cubes(V), I, Rest).

load_cubes(I) :-
    phrase(cubes(Cs), I),
    retractall(cube(_)),
    forall(member(C, Cs),
           assertz(C)).
%?- test_input(I), load_cubes(I).

surface(Area) :-
    aggregate_all(count, cube(_), CubeCount),
    aggregate_all(count,
                  ( cube(A),
                    cube(B),
                    manhattan_dist(A,B,1)
                  ),
                  AdjCount),
    Area #= CubeCount*6 - AdjCount.

test1(Ans) :-
    test_input(I),
    load_cubes(I),
    surface(Ans).
% Ans = 64

solve1(Ans) :-
    read_file_to_codes("input18.txt", I, []),
    load_cubes(I),
    surface(Ans).
% Ans = 3390
