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

counts_least_most([Z, O], 0, 1) :- Z =< O.
counts_least_most([Z, O], 1, 0) :- Z > O.

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

solve1(Report, Answer) :-
    transpose(Report, T),
    maplist(common, T, Least, Most),
    binary_number(Most, Gamma),
    binary_number(Least, Epsilon),
    Answer #= Gamma*Epsilon.

test1(Answer) :-
    test_input(Input),
    phrase(report(R), Input),
    solve1(R, Answer).
%?- test1(198).

part1(Answer) :-
    phrase_from_file(report(R), "input03.txt"),
    solve1(R, Answer).
%?- part1(Answer).
%@ Answer = 4118544 ;
%@ false.

nth0_in_list(N, V, L) :- nth0(N, L, V).

common_bit_report_partition(N, Report, Selected, Removed) :-
    transpose(Report, T),
    nth0(N, T, Col),
    common(Col, _, Most_common),
    partition(nth0_in_list(N, Most_common), Report, Selected, Removed).
%?- common_bit_report_partition(0, [[0,0,1,0,0], [1,1,1,1,0], [1,0,1,1,0]], S, R).

report_col_oxygen_rating([Row], _, Row).
report_col_oxygen_rating(Report, N, Rating) :-
    length(Report, Len),
    Len > 1,
    common_bit_report_partition(N, Report, R1, _),
    succ(N, N1),
    report_col_oxygen_rating(R1, N1, Rating).

report_col_co2_rating([Row], _, Row).
report_col_co2_rating(Report, N, Rating) :-
    length(Report, Len),
    Len > 1,
    common_bit_report_partition(N, Report, _, R1),
    succ(N, N1),
    report_col_co2_rating(R1, N1, Rating).

solve2(Report, Answer) :-
    report_col_oxygen_rating(Report, 0, Oxygen_bin),
    report_col_co2_rating(Report, 0, Co2_bin),
    binary_number(Oxygen_bin, Oxygen),
    binary_number(Co2_bin, Co2),
    Answer #= Oxygen*Co2.

test2(Answer) :-
    test_input(Input),
    phrase(report(R), Input),
    solve2(R, Answer).
%?- test2(230).

part2(Answer) :-
    phrase_from_file(report(R), "input03.txt"),
    solve2(R, Answer).
%?- part2(Answer).
%@ Answer = 3832770 ;
%@ false.
