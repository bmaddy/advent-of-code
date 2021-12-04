:- use_module(library(dcg/basics)).
:- use_module(library(listing)).
:- use_module(library(clpfd)).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(apply)).

portray_text(true).

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

%?- test1(Ans).
%@ Ans = 7 ;
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

%?- test2(Ans).
%@ Ans = 5 ;
%@ false.

solve2(Answer) :-
    phrase_from_file(ints(Parsed), "input01.txt"),
    rolling_sum(Parsed, Sums),
    ints_increasing(Sums, Answer).

%?- solve2(Ans).
%@ Ans = 1597 ;
%@ false.

%?- rolling_sum([], A).
%?- rolling_sum([1], A).
%?- rolling_sum([1,2], A).
%?- rolling_sum([1,2,3], A).
%?- rolling_sum([1,2,3,4], A).
