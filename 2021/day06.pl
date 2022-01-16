:- use_module(library(dcg/basics)).
:- use_module(library(chr)).
:- use_module(library(clpfd)).

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

%?- phrase_from_file(ints(Is), "input06.txt"), time(init(80, Is)).
%@ % 167,356,399 inferences, 18.602 CPU in 18.713 seconds (99% CPU, 8996651 Lips)
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
    %% labeling(),
    succ(DayN, Day),
    step_all(T0, T1),
    format('Day ~w\n', DayN),
    populate(DayN, T1, Count),
    true.
%?- populate(80, [3, 4, 3, 1, 2], C).

test1(Days, Answer) :-
    test_input(Input),
    phrase(ints(Is), Input),
    populate(Days, Is, Answer).
%?- test1(80, 5934).

:- table step/2.
%?- test1(80, 5934).

part1(Days, Answer) :-
    phrase_from_file(ints(Is), "input06.txt"),
    populate(Days, Is, Answer).
%?- time(part1(80, Answer)).
%@ % 31,965,624 inferences, 4.435 CPU in 4.921 seconds (90% CPU, 7206841 Lips)
%@ Answer = 363101 ;
%@ % 16 inferences, 0.015 CPU in 0.015 seconds (99% CPU, 1095 Lips)
%@ false.


%% Part 2:

%?- test1(256, Answer).

%?- part1(256, Answer).
