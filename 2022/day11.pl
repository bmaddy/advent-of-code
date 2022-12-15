:- use_module(library(chr)).

:- chr_constraint go/0, rounds/1, curr/1, have_relief/1,
                  monkey/5, item/2, inspected/2, inspection_count/2.

go, monkey(M, _, _, _, _) ==> inspection_count(M, 0).
go <=> curr(0).

curr(M), monkey(M, Op, _, _, _), have_relief(R)
\ item(M, I0),
  inspection_count(M, N0) <=> call(Op, I0, I1),
                              ( R =:= 1
                              -> I is div(I1, 3)
                              ; I is I1
                              ),
                              N is N0+1,
                              inspected(M, I),
                              inspection_count(M, N).
curr(M), monkey(M, _, D, N, _) \ inspected(M, I) <=> I mod D =:= 0 | item(N, I).
curr(M), monkey(M, _, D, _, N) \ inspected(M, I) <=> I mod D =\= 0 | item(N, I).

monkey(N, _, _, _, _) \ curr(M) <=> N =:= M+1 | curr(N).
rounds(R), curr(_) <=> R > 1 | rounds(R-1), curr(0).

mul(A, B, C) :- C is A*B.
square(A, B) :- B is A*A.

test_input :-
    maplist(item(0), [79, 98]),
    maplist(item(1), [54, 65, 75, 74]),
    maplist(item(2), [79, 60, 97]),
    maplist(item(3), [74]),

    monkey(0, mul(19), 23, 2, 3),
    monkey(1, plus(6), 19, 2, 0),
    monkey(2, square, 13, 1, 3),
    monkey(3, plus(3), 17, 0, 1).

input :-
    maplist(item(0), [74, 64, 74, 63, 53]),
    maplist(item(1), [69, 99, 95, 62]),
    maplist(item(2), [59, 81]),
    maplist(item(3), [50, 67, 63, 57, 63, 83, 97]),
    maplist(item(4), [61, 94, 85, 52, 81, 90, 94, 70]),
    maplist(item(5), [69]),
    maplist(item(6), [54, 55, 58]),
    maplist(item(7), [79, 51, 83, 88, 93, 76]),

    monkey(0, mul(7),   5, 1, 6),
    monkey(1, square,  17, 2, 5),
    monkey(2, plus(8),  7, 4, 3),
    monkey(3, plus(4), 13, 0, 7),
    monkey(4, plus(3), 19, 7, 3),
    monkey(5, plus(5),  3, 4, 2),
    monkey(6, plus(7), 11, 1, 5),
    monkey(7, mul(3),   2, 0, 6).

test1 :-
    test_input,
    have_relief(1),
    rounds(20),
    go.
% 101*105 = 10605

solve1 :-
    input,
    have_relief(1),
    rounds(20),
    go.
% 231*234 = 54054

test2 :-
    test_input,
    have_relief(0),
    rounds(1000),
    go.

solve2 :-
    input,
    have_relief(0),
    rounds(20),
    go.
