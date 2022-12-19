:- use_module(library(dcg/basics)).
:- autoload(library(readutil), [ read_file_to_codes/3
                               ]).

:- use_module(library(clpfd)).
:- use_module(util, [ manhattan_dist/3 ]).

:- autoload(library(lists), [ member/2,
                              subtract/3
                            ]).

:- autoload(library(aggregate), [ aggregate_all/3
                                ]).

% cd("/home/bmaddy/src/bmaddy/adventofcode/2022").

test_input(`Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3
`).

sensor(s(pos(Sx,Sy),
         pos(Bx,By))) --> "Sensor at x=", number(Sx),
                          ", y=", number(Sy),
                          ": closest beacon is at x=", number(Bx),
                          ", y=", number(By).
%?- test_input(I), phrase(sensor(S), I, Rest).

sensors([]) --> eos.
sensors([S|Ss]) --> sensor(S), "\n", sensors(Ss).
%?- test_input(I), phrase(sensors(Ss), I, Rest).

load_sensors(I) :-
    phrase(sensors(Ss), I),
    retractall(s(_, _)),
    forall(member(S, Ss),
           assertz(S)).
%?- test_input(I), load_sensors(I).

% sensor_covered(S, P) :-
%     s(S, B),
%     manhattan_dist(S, B, Range),
%     manhattan_dist(S, P, D),
%     D #=< Range.

sensor_covered(pos(Sx,Sy), pos(Px,Py)) :-
    s(pos(Sx,Sy), pos(Bx,By)),
    manhattan_dist(pos(Sx,Sy), pos(Bx,By), Range),
    manhattan_dist(pos(Sx,Sy), pos(Px,Py), D),
    % D #=< Range.
    D #=< Range,
    labeling([ff], [Sx,Sy,Px,Py,Bx,By,Range,D]).
%?- sensor_covered(S, pos(12,2)).
%?- sensor_covered(_, pos(X,10)).

row_empty_count(Y, C) :-
    aggregate_all(set(X),
                  ( sensor_covered(_, pos(X,Y)),
                    label([X])
                    % labeling([ff], [X])
                  ),
                  Covered),
    aggregate_all(set(X),
                  s(_, pos(X,Y)),
                  Taken),
    subtract(Covered, Taken, Empty),
    length(Empty, C).

test1(Ans) :-
    test_input(I),
    load_sensors(I),
    row_empty_count(10, Ans).
% Ans = 26 

solve1(Ans) :-
    read_file_to_codes("input15.txt", I, []),
    load_sensors(I),
    row_empty_count(10, Ans).
