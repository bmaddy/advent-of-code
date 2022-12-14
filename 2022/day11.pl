:- use_module(library(chr)).

:- chr_constraint round/1, curr/1, monkey/5, item/2, inspected/2.

curr(M), monkey(M, Op, _, _, _) \ item(M, I0) <=> call(Op, I0, I1),
                                                  I is div(I1, 3),
                                                  inspected(M, I).
curr(M), monkey(M, _, D, N, _) \ inspected(M, I) <=> I mod D =:= 0 | item(N, I).
curr(M), monkey(M, _, D, _, N) \ inspected(M, I) <=> I mod D =\= 0 | item(N, I).

monkey(N, _, _, _, _) \ curr(M) <=> N =:= M+1 | curr(N).
round(R), curr(_) <=> R < 20 | round(R+1), curr(0).

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
    monkey(3, plus(3), 17, 0, 1),

    round(1),
    curr(0).
