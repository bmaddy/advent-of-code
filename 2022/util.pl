:- module(util, [setup/0
                ]).
:- use_module(library(clpfd)).

manhattan_dist(pos(X0,Y0), pos(X1,Y1), D) :-
    D #= abs(X1 - X0) + abs(Y1 - Y0).
%?- manhattan_dist(pos(0,0), pos(1,2), D).
%?- manhattan_dist(pos(0,0), pos(-1,1), D).
%?- manhattan_dist(pos(0,0), pos(X,0), 2), label([X]).
%?- manhattan_dist(pos(0,0), pos(X,Y), 1), label([X,Y]).
manhattan_dist(pos(X0,Y0,Z0), pos(X1,Y1,Z1), D) :-
    D #= abs(X1 - X0) + abs(Y1 - Y0) + abs(Z1 - Z0).
%?- manhattan_dist(pos(0,0,0), pos(1,2,-3), D).
%?- manhattan_dist(pos(0,0,0), pos(X,0,0), 2), label([X]).
%?- manhattan_dist(pos(0,0,0), pos(X,Y,Z), 1), label([X,Y,Z]).
