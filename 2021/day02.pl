:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

%?- portray_text(true).

test_input(`forward 5
down 5
forward 8
up 3
down 8
forward 2`).

commands([]) --> [].
commands([move(D,A)|Cs]) --> nonblanks(D), blanks, integer(A),
                         blanks, commands(Cs).

move_from_to(move(`forward`,A), pos(X0,Y0), pos(X0,Y1)) :- Y1 #= Y0 + A.
move_from_to(move(`down`,A), pos(X0,Y0), pos(X1,Y0)) :- X1 #= X0 + A.
move_from_to(move(`up`,A), pos(X0,Y0), pos(X1,Y0)) :- X1 #= X0 - A.

part1(Commands, Answer) :-
    foldl(move_from_to, Commands, pos(0,0), pos(Depth, Horizontal_position)),
    Answer #= Horizontal_position * Depth.

test1(Answer) :-
    test_input(Input),
    phrase(commands(Cs), Input),
    part1(Cs, Answer).
%?- test1(150).

solve1(Answer) :-
    phrase_from_file(commands(Cs), `input02.txt`),
    part1(Cs, Answer).
%?- solve1(Answer).
%@ Answer = 1692075 ;
%@ false.

aimed_move_from_to(move(`forward`,A), pos(X0,Y0,A0), pos(X1,Y1,A0)) :-
    X1 #= X0 + A*A0,
    Y1 #= Y0 + A.
aimed_move_from_to(move(`down`,A), pos(X0,Y0,A0), pos(X0,Y0,A1)) :- A1 #= A0 + A.
aimed_move_from_to(move(`up`,A), pos(X0,Y0,A0), pos(X0,Y0,A1)) :- A1 #= A0 - A.

part2(Commands, Answer) :-
    foldl(aimed_move_from_to, Commands, pos(0,0,0), pos(Depth, Horizontal_position, _)),
    Answer #= Horizontal_position * Depth.

test2(Answer) :-
    test_input(Input),
    phrase(commands(Cs), Input),
    part2(Cs, Answer).
%?- test2(900).

solve2(Answer) :-
    phrase_from_file(commands(Cs), `input02.txt`),
    part2(Cs, Answer).
%?- solve2(Answer).
%@ Answer = 1749524700 ;
%@ false.
