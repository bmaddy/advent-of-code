:- use_module(library(dcg/basics)).
:- use_module(library(pio)).
:- use_module(library(apply)).

policy(Low, High, Letter) -->  integer(Low), "-", integer(High), " ", nonblank(Letter).

end_of_string([], []).

line(password(Low, High, Letter, Password)) -->
    policy(Low, High, Letter), ": ", nonblanks(Password), ( "\n" | call(end_of_string) ).

valid_password(password(Low, High, C, P)) :-
    include(same_term(C), P, Cs),
    string_length(Cs, Count),
    between(Low, High, Count).
%?- valid_password(password(1, 3, 0'a, `abcde`)).
%% true.
%?- valid_password(password(1, 3, 0'b, `cdefg`)).
%% false.
%?- valid_password(password(2, 9, 0'c, `ccccccccc`)).
%% true.

lines([L|Ls]) -->
    line(L),
    !,
    lines(Ls).
lines([]) --> [].

count_valid(N, Ls) :-
    include(valid_password, Ls, Vs),
    length(Vs, N).

part_1_test(N) :-
    phrase( lines(Ls), `1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc`),
    count_valid(N, Ls).
%?- part_1_test(Valid).
%% Valid = 2.

part_1(N) :-
    phrase_from_file(lines(Ls), 'input02.txt'),
    count_valid(N, Ls).
%?- part_1(Valid).
%% Valid = 625.



valid_positional_password(password(P1, P2, L, Pass)) :-
    nth1(P1, Pass, L), not(nth1(P2, Pass, L))
    ; nth1(P2, Pass, L), not(nth1(P1, Pass, L)).
%?- valid_positional_password(password(1, 3, 0'a, `abcde`)).
%% true.
%?- valid_positional_password(password(1, 3, 0'b, `cdefg`)).
%% false.
%?- valid_positional_password(password(2, 9, 0'c, `ccccccccc`)).
%% false.

part_2_test(N) :-
    phrase( lines(Ls), `1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc`),
    include(valid_positional_password, Ls, Vs),
    length(Vs, N).
%?- part_2_test(Valid).
%% Valid = 1.

part_2(N) :-
    phrase_from_file( lines(Ls), 'input02.txt'),
    include(valid_positional_password, Ls, Vs),
    length(Vs, N).
%?- part_2(Valid).
%% Valid = 391.
