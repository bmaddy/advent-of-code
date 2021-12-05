:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

%?- portray_text(true).

test_input(`199
200
208
210
200
207
240
269
260
263`).

ints([]) --> [].
ints([D|Ds]) --> integer(D), blanks, ints(Ds).

ints_increasing(Parsed, Answer) :-
    findall(_,
            ( nextto(A,B,Parsed),
              A #< B
            ),
            Filtered),
    length(Filtered, Answer).

test1(Answer) :-
    test_input(Input),
    phrase(ints(Parsed), Input),
    ints_increasing(Parsed, Answer).

solve1(Answer) :-
    phrase_from_file(ints(Parsed), "input01.txt"),
    ints_increasing(Parsed, Answer).

%?- test1(7).
%@ true ;
%@ false.

%?- solve1(Ans).
%@ Ans = 1553 ;
%@ false.

rolling_sum([], []).
rolling_sum([_], []).
rolling_sum([_,_], []).
rolling_sum([A,B,C|T], [Sum|Sums]) :-
    Sum #= A + B + C,
    rolling_sum([B,C|T], Sums).

test2(Answer) :-
    test_input(Input),
    phrase(ints(Parsed), Input),
    rolling_sum(Parsed, Sums),
    ints_increasing(Sums, Answer).

%?- test2(5).
%@ true ;
%@ false.

solve2(Answer) :-
    phrase_from_file(ints(Parsed), "input01.txt"),
    rolling_sum(Parsed, Sums),
    ints_increasing(Sums, Answer).

%?- solve2(Ans).
%@ Ans = 1597 ;
%@ false.
