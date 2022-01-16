:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

test_input(`16,1,2,0,4,2,7,1,2,14`).

cost(_, [], 0).
cost(Target, [P|Ps], C) :-
    Target #= N + P,
    cost(Target, Ps, C1),
    C #= C1 + abs(N).
%?- cost(2, [16,1,2,0,4,2,7,1,2,14], 37).
%?- cost(1, [16,1,2,0,4,2,7,1,2,14], 41).
%?- cost(3, [16,1,2,0,4,2,7,1,2,14], 39).
%?- cost(10, [16,1,2,0,4,2,7,1,2,14], 71).

%?- T in 0..16, cost(T, [16,1,2,0,4,2,7,1,2,14,16,1,2,0,4,2,7,1,2,14,16,1,2,0,4,2,7,1,2,14,16,1,2,0,4,2,7,1,2,14], C), time(labeling([min(C), ff], [C, T])).
%@ % 857,329 inferences, 0.103 CPU in 0.117 seconds (88% CPU, 8335074 Lips)
%@ T = 2,
%@ C = 148 
%@ % Break level 9

%?- L = [16,1,2,0,4,2,7,1,2,14,16,1,2,0,4,2,7,1,2,14,16,1,2,0,4,2,7,1,2,14,16,1,2,0,4,2,7,1,2,14], time(solve1(L, Target, Cost)).

%% Parsing
ints([I]) --> integer(I), blanks, eos.
ints([I|Is]) --> integer(I), `,`, ints(Is).
%?- test_input(I), phrase(ints(Is), I).

solve1(Locations, Target, Cost) :-
    min_member(Min, Locations),
    max_member(Max, Locations),
    Target in Min..Max,
    cost(Target, Locations, Cost),
    once(labeling([min(Cost), ff], [Cost, Target])).
%?- solve1([16,1,2,0,4,2,7,1,2,14], Target, Cost).

test1(Target, Cost) :-
    test_input(Input),
    phrase(ints(Locations), Input),
    solve1(Locations, Target, Cost).
%?- time(test1(Target, Cost)).
%@ Target = 2,
%@ Cost = 37 ;
%@ false.
%?- test1(1, 41).
%?- test1(3, 39).
%?- test1(10, 71).

part1(Target, Cost) :-
    phrase_from_file(ints(Locations), "input07.txt"),
    solve1(Locations, Target, Cost).
%?- part1(Target, Cost).
