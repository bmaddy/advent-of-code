:- use_module(library(dcg/basics)).
:- use_module(library(listing)).
:- use_module(library(clpfd)).
:- use_module(library(readutil), [read_line_to_string/2]).
:- use_module(library(apply)).

portray_text(true).

test1_input(`199
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
    test1_input(Input),
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
