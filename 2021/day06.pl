:- use_module(library(dcg/basics)).
:- use_module(library(chr)).

:- chr_constraint fish/2, init/2, count/1.

fish(Day, Timer) <=> 0 < Day, 0 < Timer | succ(NDay, Day), succ(NTimer, Timer), fish(NDay, NTimer).
fish(Day, 0)     <=> 0 < Day | succ(NDay, Day), fish(NDay, 6), fish(NDay, 8).
%?- fish(5, 3).

init(Day, [F]) <=> fish(Day, F).
init(Day, [F|Fs]) <=> fish(Day, F), init(Day, Fs).
%?- init(5, [3]).

fish(0, _) <=> count(1).
count(A), count(B) <=> N is A + B, count(N).

test_input(`3,4,3,1,2`).

ints([I]) --> integer(I), blanks, eos.
ints([I|Is]) --> integer(I), `,`, ints(Is).
%?- test_input(I), phrase(ints(Is), I).

%% test1(Answer) :-
%%     test_input(Input),
%%     phrase(ints(Is), Input),
%%     init(80, Is).
%?- test1(Ans).
%?- init(18, [3, 4, 3, 1, 2]).
%?- init(80, [3, 4, 3, 1, 2]).
%?- aggregate_all(count, init(18, [3, 4, 3, 1, 2]), Count).

%% Part 1:

%?- test_input(I), phrase(ints(Is), I), init(80, Is).
%@ I = [51, 44, 52, 44, 51, 44, 49, 44, 50],
%@ Is = [3, 4, 3, 1, 2],
%@ count(5934) ;
%@ false.

%?- phrase_from_file(ints(Is), "input06.txt"), init(80, Is).
%@ Is = [1, 3, 3, 4, 5, 1, 1, 1, 1|...],
%@ count(363101) ;
%@ false.

step(0, [6, 8]).
step(T0, [T1]) :- succ(T1, T0).
step_all([], []).
step_all([H0|T0], Updated) :-
    step(H0, L),
    step_all(T0, T),
    append(L, T, Updated).
%?- step_all([3,4,3,1,2], U).

populate(0, Timers, Count) :- length(Timers, Count).
populate(Day, T0, Count) :-
    succ(DayN, Day),
    step_all(T0, T1),
    populate(DayN, T1, Count).
%?- populate(80, [3, 4, 3, 1, 2], C).

test1(Answer) :-
    test_input(Input),
    phrase(ints(Is), Input),
    populate(80, Is, Answer).
%?- test1(5934).

part1(Answer) :-
    phrase_from_file(ints(Is), "input06.txt"),
    populate(80, Is, Answer).
%?- part1(Answer).
%@ ERROR: Stack limit (1.0Gb) exceeded

%% Part 2:

%?- test_input(I), phrase(ints(Is), I), init(256, Is).

%?- phrase_from_file(ints(Is), "input06.txt"), init(256, Is).
