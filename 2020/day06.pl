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


person([H|T]) --> nonblank(C), person(T), { atom_char(H, C) }, !.
person([H]) --> nonblank(C), { atom_char(H, C) }.
%% ?- phrase(person(Qs), `abc`, Rest).
%% Qs = "abc",
%% Rest = [].

group([person(H)|T]) --> person(H), "\n", group(T), !.
group([person(P)]) --> person(P).
%% ?- phrase(group(G), `a
%%    b
%%    c`, Rest).
%% |    G = [person([97]), person([98]), person([99])],
%%      Rest = [].

groups([group(H)|T]) --> group(H), "\n\n", groups(T).
groups([group(G)]) --> group(G), ( "\n" | [] ), end_of_string.
%% ?- test_input(I), phrase(groups(Gs), I, Rest).
%% Gs = [group([person([a, b, c])]), group([person([a]), person([b]), person([c])]), group([person([a, b]), person([a, c])]), group([person([a]), person([a]), person([...]), person(...)]), group([person([b])])],
%% Rest = [] ;
%% false.

unanimous_yeses([], []).
unanimous_yeses([person(P)], P).
unanimous_yeses([person(P)|Ps], Ys) :-
    unanimous_yeses(Ps, T),
    intersection(P, T, Ys).

unanimous_count_sum([], 0).
unanimous_count_sum([group(Ps)|Gs], N) :-
    unanimous_yeses(Ps, Ys),
    length(Ys, N1),
    unanimous_count_sum(Gs, N2),
    plus(N1, N2, N),
    !.

part_2_test(N) :-
    test_input(I),
    phrase(groups(Gs), I),
    unanimous_count_sum(Gs, N).
%% ?- part_2_test(N).
%% N = 6 ;
%% false.

part_2(N) :-
    phrase_from_file(groups(Gs), 'input06.txt'),
    unanimous_count_sum(Gs, N).
%% ?- part_2(N).
%% N = 3437 ;
%% false.
