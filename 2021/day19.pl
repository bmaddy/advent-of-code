:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

%?- portray_text(true).

test_input(`--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361`).

coord3(X,Y,Z) --> integer(X), ",", integer(Y), ",", integer(Z).
%?- phrase(coord3(X,Y,Z), `1,-2,3`, Rest).

coords3([pos(X,Y,Z)]) --> coord3(X,Y,Z).
coords3([pos(X,Y,Z)|Cs]) --> coord3(X,Y,Z), "\n", coords3(Cs).
%?- phrase(coords3(Cs), `-1,-1,1\n-2,-2,2`, Rest).

header(Id) --> "--- scanner ", integer(Id), " ---\n".
%?- test_input(I), phrase(header(Id), I, Rest).

beacon(Id, scanner_detection(Id, pos(X,Y,Z))) --> coord3(X,Y,Z).

beacons(Id, [B]) --> beacon(Id, B).
beacons(Id, [B|Bs]) -->
    beacon(Id, B),
    "\n",
    beacons(Id, Bs).
%?- phrase(beacons(0, B), `-1,-1,1\n-2,-2,2\n\n--- scanner 0 ---\n-1,-1,1`, Rest).

scanner(Bs) -->
    header(Id),
    beacons(Id, Bs).

scanners(Bs) --> scanner(Bs).
scanners(Bs) -->
    scanner(A),
    "\n\n",
    scanners(B),
    { append(A, B, Bs) }.
%?- phrase(scanners(B), `--- scanner 0 ---\n-1,-1,1\n-2,-2,2\n\n--- scanner 1 ---\n-1,-1,1`, Rest).
%?- test_input(I), phrase(scanners(Bs), I, Rest).
%?- test_input(I), phrase((scanners(Bs), eol), I).
%?- phrase_from_file((scanners(Bs), eol), "input19-test.txt").
%?- phrase_from_file((scanners(Bs), eol), "input19.txt").

assert_beacons(Bs) :-
    retractall(scanner_detection(_, _)),
    maplist(assertz, Bs).

%% solve1(File, Answer) :-
%%     phrase_from_file(scanners(Bs), File),
%%     assert_beacons(Bs), !.
%?- solve1("input19-test.txt", Ans).

test1() :-
    test_input(I),
    phrase(scanners(Bs), I),
    %% phrase(scanners(Bs), `--- scanner 0 ---\n-1,-1,1\n-2,-2,2\n\n--- scanner 1 ---\n-1,-1,1`),
    assert_beacons(Bs), !.
%?- test1().
%?- findall(P, scanner_detection(4, P), All), length(All, Len).

% ERROR: major bug here, see 2022/util.pl:manhattan_dist
manhattan_dist(pos(X0,Y0,Z0), pos(X1,Y1,Z1), D) :-
    plus(X0, Dx, X1),
    plus(Y0, Dy, Y1),
    plus(Z0, Dz, Z1),
    D #= Dx + Dy + Dz.
%?- manhattan_dist(pos(0,0,0), pos(1,2,3), D).

product_sum(A, B, Init, Init + A*B).
%% product_sum(A, B, Init, Sum) :- Sum #= Init + A*B.
dot_product([A|As], [B|Bs], C) :-
    foldl(product_sum, As, Bs, A*B, C).
%?- dot_product([1,2,3], [4,5,6], C).

row_multiply(Cols, Row, Result) :-
    maplist(dot_product(Row), Cols, Result).
%?- row_multiply([1,2], [[1,1], [1,1]], R).

%% adapted from https://stackoverflow.com/questions/34206275/matrix-multiplication-with-prolog/34209855#34209855
matrix_multiply(A, B, C) :-
    transpose(B, BT),
    maplist(row_multiply(BT), A, C).
%?- matrix_multiply([[1,2],[3,4],[5,6]], [[1,1,1],[1,1,1]],R),maplist(maplist(is),C,R).
%@ R = [[1*1+2*1, 1*1+2*1, 1*1+2*1], [3*1+4*1, 3*1+4*1, 3*1+4*1], [5*1+6*1, 5*1+6*1, 5*1+6*1]],
%@ C = [[3, 3, 3], [7, 7, 7], [11, 11, 11]].

pos_matrix(pos(X,Y,Z), [[X], [Y], [Z]]).

identity([[1,0,0],
          [0,1,0],
          [0,0,1]]).

rotate90(x, [[1,0,0],
             [0,0,-1],
             [0,1,0]]).
rotate90(y, [[0,0,1],
             [0,1,0],
             [-1,0,0]]).
rotate90(z, [[0,-1,0],
             [1,0,0],
             [0,0,1]]).
rotate180(Axis, M) :-
    rotate90(Axis, Quarter),
    matrix_multiply(Quarter, Quarter, M).

matrix_is(E, M) :- maplist(maplist(is), E, M).

rotations(Axis, M) :- rotations(Axis, 4, M).
rotations(_, N, [[1,0,0],
                 [0,1,0],
                 [0,0,1]]) :- 0 #< N.
rotations(Axis, N, M) :-
    rotate90(Axis, Quarter),
    succ(Next, N),
    rotations(Axis, Next, Identity),
    matrix_multiply(Quarter, Identity, Evaluated),
    matrix_is(M, Evaluated).
%?- time(findall(M, rotations(x, M), Bag)).


faces(M) :- rotate90(z, M).
faces(M) :-
    rotate90(z, Quarter),
    rotate180(z, Half),
    matrix_multiply(Quarter, Half, E),
    matrix_is(M, E).
faces(M) :- rotations(x, M).
%?- faces(M).

%?- pos_matrix(pos(1,2,3), P), faces(T), matrix_multiply(T, P, M), matrix_is(E, M).

%% pos_translations(_, _)
