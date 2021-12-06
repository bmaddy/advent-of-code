:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

%?- portray_text(true).

test_input(`00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010`).

bit(0) --> "0".
bit(1) --> "1".

bits([B]) --> bit(B), eol.
bits([B|Bs]) --> bit(B), bits(Bs).
%?- phrase(bits(Bs), `111`, Rest).

report([]) --> [].
report([H|T]) --> bits(H), report(T).
%?- phrase(report(R), `111\n000`), transpose(R, T).
%?- test_input(I), phrase(report(R), I).

counts_least_most([Z, O], 0, 1) :- Z < O.
counts_least_most([Z, O], 1, 0) :- Z >= O.

common(L, Least, Most) :-
    partition(=(0), L, Zs, Os),
    length(Zs, Zero_count),
    length(Os, One_count),
    counts_least_most([Zero_count, One_count], Least, Most).
%?- common([1,0,1,1,0], L, M).

binary_number([B], B).
binary_number([B|Bs], N) :-
    length(Bs, Exp),
    binary_number(Bs, N0),
    N #= B*2^Exp + N0.
%?- binary_number([1,0,1,1,0], N).

part1(Report, Answer) :-
    transpose(Report, T),
    maplist(common, T, Least, Most),
    binary_number(Most, Gamma),
    binary_number(Least, Epsilon),
    Answer #= Gamma*Epsilon.

test1(Answer) :-
    test_input(Input),
    phrase(report(R), Input),
    part1(R, Answer).
%?- test1(198).

solve1(Answer) :-
    phrase_from_file(report(R), "input03.txt"),
    part1(R, Answer).
%?- solve1(Answer).
%@ Answer = 4118544 ;
%@ false.
