:- use_module(library(dcg/basics)).
:- autoload(library(apply), [ maplist/3
                            ]).

:- autoload(library(pure_input), [ phrase_from_file/2
                                 ]).

:- autoload(library(portray_text), [ portray_text/1
                                   ]).

:- autoload(library(lists), [ member/2,
                              max_list/2
                            ]).

:- use_module(library(clpfd)).

setup :-
    portray_text(true),
    working_directory(_, "/home/bmaddy/src/bmaddy/adventofcode/2022").

% ... --> [] | [_], ... .

test_input(`1000
2000
3000

4000

5000
6000

7000
8000
9000

10000`).

% try getting it into [[1000, 2000, 3000], [4000], ...] then use somthing like nth0/3

elf([S]) --> number(S).
elf([S|Ss]) --> number(S), eol, elf(Ss).
%?- test_input(Input), phrase(elf(S), Input, Rest).
%?- phrase(elf(S), `10000`, Rest).

elves([E]) --> elf(E), ("\n" | []).
elves([E|Es]) --> elf(E), "\n\n", elves(Es).
%?- test_input(Input), phrase(elves(E), Input, Rest).
%?- phrase_from_file(elves(E), "input01.txt").

% tail([T,A,I,L]) --> string(_), [T,A,I,L], eos.
%?- test_input(Input), phrase(tail(E), Input, Rest).
%?- phrase_from_file(tail(E), "input01.txt").

% elves_facts([], [], _).
% elves_facts([[S|Ss]|Es], [elf_snack(N, S)|Fs], N) :-
%     elves_facts([Ss|Es], Fs, N).
% elves_facts([[]|Es], Fs, N0) :-
%     succ(N0, N),
%     elves_facts(Es, Fs, N).
% elves_facts(Es, Fs) :- elves_facts(Es, Fs, 0).
% %?- elves_facts([], Fs, 0).
% %?- elves_facts([[11]], Fs, 0).
% %?- elves_facts([[11, 22], [33, 44]], Fs, 0).
% %?- elves_facts([[11, 22], [33, 44]], Fs).

total_calories(L, T) :- sum(L, #=, T).

elves_sorted_calories(Elves, Cs) :-
    maplist(total_calories, Elves, Unsorted),
    sort(Unsorted, Sorted),
    reverse(Sorted, Cs).

test1(Ans) :-
    test_input(Input),
    phrase(elves(Elves), Input),
    elves_sorted_calories(Elves, [Ans|_]).
%?- test1(Ans).
% Ans = 24000

solve1(Ans) :-
    phrase_from_file(elves(Elves), "input01.txt"),
    elves_sorted_calories(Elves, [Ans|_]).
%?- solve1(Ans).
% Ans = 68467

% :- dynamic elf_snack/2.

%     % abandoned assert method
%     % elves_facts(Elves, Facts),
%     % retractall(elf_snack(_,_)),
%     % forall(member(Fact, Facts),
%     %       assertz(Fact)),

test2(Ans) :-
    test_input(Input),
    phrase(elves(Es), Input),
    elves_sorted_calories(Es, [A,B,C|_]),
    Ans #= A + B + C.
%?- test2(Ans).
% Ans = 45000

solve2(Ans) :-
    phrase_from_file(elves(Elves), "input01.txt"),
    elves_sorted_calories(Elves, [A,B,C|_]),
    Ans #= A + B + C.
%?- solve2(Ans).
% Ans = 203420
