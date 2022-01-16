:- use_module(library(clpfd)).

tick_x_vel(0, 0).
tick_x_vel(V0, V1) :- V0 #< 0, V1 #= V0 + 1.
tick_x_vel(V0, V1) :- V0 #> 0, V1 #= V0 - 1.
%?- tick_x_vel(7, V).

tick(pos(X0,Y0), vel(Vx0, Vy0), pos(X1,Y1), vel(Vx1,Vy1)) :-
    X1 #= X0 + Vx0,
    Y1 #= Y0 + Vy0,
    tick_x_vel(Vx0, Vx1),
    Vy1 #= Vy0 - 1.
%?- tick(pos(0,0), vel(7,2), P, V).

step(0, P, V, P, V).
step(N0, P0, V0, Pn, Vn) :-
    0 < N0,
    tick(P0, V0, P1, V1),
    N1 #= N0 - 1,
    step(N1, P1, V1, Pn, Vn).
%?- step(7, pos(0,0), vel(7,2), P, V).

%% target(pos(X,Y), _, Tx, Ty) :-
%%     X in Tx,
%%     Y in Ty.
%% target(P0, V0, Tx, Ty) :-
%%     tick(P0, V0, P1, V1),
%%     target(P1, V1, Tx, Ty).

target(pos(X,Y), _, Y, X, Y).
target(pos(X0,Y0), V0, MaxY, Tx, Ty) :-
    tick(pos(X0,Y0), V0, P1, V1),
    MaxY #= max(Y0, MaxY1),
    target(P1, V1, MaxY1, Tx, Ty).
%?- target(pos(0,0), vel(7,2), MaxY, Tx, Ty), Tx in 20..30, Ty in -10..(-5).
%?- target(pos(0,0), vel(6,9), MaxY, Tx, Ty), Tx in 20..30, Ty in -10..(-5).

%% :- table step/5.
%?- target(pos(0,0), vel(X,Y), MaxY, Tx, Ty), Tx in 20..30, Ty in -10..(-5), labeling([ff, min(abs(Tx) + abs(Ty)), max(MaxY)], [MaxY,Tx,Ty]).
