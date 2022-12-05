:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

%?- portray_text(true).

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

... --> [] | [_], ... .
%?- phrase((...,[Last]), "hello").
%?- phrase((...,[E,E],...), Ls).

% try getting it into [[1000, 2000, 3000], [4000], ...] then use somthing like nth0/3

elf([S]) --> number(S).
elf([S|Ss]) --> number(S), eol, elf(Ss).
%?- test_input(Input), phrase(elf(S), Input, Rest).
%?- phrase(elf(S), `10000`, Rest).

elves([E]) --> elf(E).
elves([E|Es]) --> elf(E), "\n\n", elves(Es).
%?- test_input(Input), phrase(elves(E), Input, Rest).

test1(Ans) :-
    test_input(Input),
    phrase(elves(Elves), Input),
    nth0(I, Elves, Ans).
