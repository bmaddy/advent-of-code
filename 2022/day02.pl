% https://github.com/hakank/hakank/blob/master/advent-of-code-2022/2_swi.pl

:- use_module(library(dcg/basics)).

% cd("/home/bmaddy/src/bmaddy/adventofcode/2022").

test_input(`A Y
B X
C Z
`).

shape(rock) --> "A" | "X".
shape(paper) --> "B" | "Y".
shape(scissors) --> "C" | "Z".

play(play(Them, You)) --> shape(Them), " ", shape(You).
%?- test_input(I), phrase(play(P), I, Rest).

guide([]) --> eos.
guide([P|Ps]) --> play(P), eol, guide(Ps).
%?- test_input(I), phrase(guide(G), I, Rest).

shape_score(rock, 1).
shape_score(paper, 2).
shape_score(scissors, 3).

lose(rock, scissors).
lose(paper, rock).
lose(scissors, paper).
draw(A,A).
win(T, Y) :- lose(Y, T).

round_score(S, S, 3).
round_score(rock, paper, 6).
round_score(rock, scissors, 0).
round_score(paper, rock, 0).
round_score(paper, scissors, 6).
round_score(scissors, rock, 6).
round_score(scissors, paper, 0).

play_score(play(T,Y), S) :-
    shape_score(Y, SS),
    round_score(T, Y, RS),
    plus(SS, RS, S).
%?- play_score(play(rock, paper), S).

guide_score([], 0).
guide_score([P|Ps], S) :-
    guide_score(Ps, S1),
    play_score(P, S0),
    plus(S0, S1, S).
%?- test_input(I), phrase(guide(G), I), guide_score(G, S).

test1(Ans) :-
    test_input(I),
    phrase(guide(G), I),
    guide_score(G, Ans).

solve1(Ans) :-
    read_file_to_codes("input02.txt", I, []),
    phrase(guide(G), I),
    guide_score(G, Ans).
%?- solve1(Ans).
% Ans = 12772
