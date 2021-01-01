:- use_module(library(dcg/basics)).

%% ?- portray_text(true).

test_input(`abc

a
b
c

ab
ac

a
a
a
a

b`).

chars([C|Cs]) --> nonblank(C), ( "\n" | [] ), chars(Cs), !.
chars([C]) --> nonblank(C).
%% ?- test_input(I), phrase(chars(Cs), I, Rest).

end_of_string([], []).

groups([H|T]) --> chars(H), "\n\n", groups(T), !.
groups([H]) --> chars(H), ( "\n" | [] ), end_of_string.

unique_letter_count(N) --> chars(Cs), "\n\n", unique_letter_count(N2), !, { list_to_set(Cs, S), length(S, N1), plus(N1, N2, N) }.
unique_letter_count(N) --> chars(Cs), ( "\n" | [] ), end_of_string, { list_to_set(Cs, S), length(S, N) }.

part_1_test(N) :-
    test_input(I),
    phrase(unique_letter_count(N), I).
%% ?- part_1_test(N).
%% N = 11.

part_1(N) :-
    phrase_from_file(unique_letter_count(N), 'input06.txt').
%% ?- part_1(N).
%% N = 6630.
