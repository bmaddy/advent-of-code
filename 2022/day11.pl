:- use_module(library(chr)).

:- chr_constraint round/1, monkey/1, item/2, inspected/2.

monkey(0) \ item(0, I0) <=> I is div(I0*19, 3), inspected(0, I).
monkey(0) \ inspected(0, I) <=> I mod 23 =:= 0 | item(2, I).
monkey(0) \ inspected(0, I) <=> I mod 23 =\= 0 | item(3, I).

monkey(0) <=> monkey(1).

monkey(1) \ item(1, I0) <=> I is div(I0+6, 3), inspected(1, I).
monkey(1) \ inspected(1, I) <=> I mod 19 =:= 0 | item(2, I).
monkey(1) \ inspected(1, I) <=> I mod 19 =\= 0 | item(0, I).

monkey(1) <=> monkey(2).

monkey(2) \ item(2, I0) <=> I is div(I0*I0, 3), inspected(2, I).
monkey(2) \ inspected(2, I) <=> I mod 13 =:= 0 | item(1, I).
monkey(2) \ inspected(2, I) <=> I mod 13 =\= 0 | item(3, I).

monkey(2) <=> monkey(3).

monkey(3) \ item(3, I0) <=> I is div(I0+3, 3), inspected(3, I).
monkey(3) \ inspected(3, I) <=> I mod 17 =:= 0 | item(0, I).
monkey(3) \ inspected(3, I) <=> I mod 17 =\= 0 | item(1, I).

round(N), monkey(3) <=> N < 20 | round(N+1), monkey(0).

test_input :-
    maplist(item(0), [79, 98]),
    maplist(item(1), [54, 65, 75, 74]),
    maplist(item(2), [79, 60, 97]),
    maplist(item(3), [74]),

    round(1),
    monkey(0).
