:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util, [ manhattan_dist/3 ]).

:- autoload(library(lists), [ member/2
                            ]).

:- autoload(library(aggregate), [ aggregate_all/3
                                ]).

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
Sensor at x=20, y=1: closest beacon is at x=15, y=3`).

sensor(s(pos(Sx,Sy),
         pos(Bx,By))) --> "Sensor at x=", number(Sx),
                          ", y=", number(Sy),
                          ": closest beacon is at x=", number(Bx),
                          ", y=", number(By).
%?- test_input(I), phrase(sensor(S), I, Rest).

sensors([S]) --> sensor(S), eos.
sensors([S|Ss]) --> sensor(S), "\n", sensors(Ss).
%?- test_input(I), phrase(sensors(Ss), I, Rest).

load_sensors(I) :-
    phrase(sensors(Ss), I),
    retractall(s(_, _)),
    forall(member(S, Ss),
           assertz(S)).
%?- test_input(I), load_sensors(I).

sensor_covered(S, P) :-
    s(S, B),
    manhattan_dist(S, B, Range),
    manhattan_dist(S, P, D),
    D #=< Range.
%?- sensor_covered(S, pos(12,2)).
%?- sensor_covered(_, pos(X,10)).

test1(Ans) :-
    test_input(I),
    load_sensors(I),
    aggregate_all(set(X),
                  ( sensor_covered(_, pos(X,10)),
                    % \+ s(_, pos(X,10)),
                    label([X])
                  ),
                  % Ans).
                  Covered),
    aggregate_all(set(X),
                  s(_, pos(X,10)),
                  Taken),
    subtract(Covered, Taken, Empty),
    length(Empty, Ans).
