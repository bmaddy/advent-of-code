:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

%?- portray_text(true).

test_input(`--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14`).

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

assert_beacons(Bs) :-
    retractall(scanner_detection(_, _)),
    maplist(assertz, Bs).

test1() :-
    test_input(I),
    phrase(scanners(Bs), I),
    %% phrase(scanners(Bs), `--- scanner 0 ---\n-1,-1,1\n-2,-2,2\n\n--- scanner 1 ---\n-1,-1,1`),
    assert_beacons(Bs), !.
%?- test1().
%?- findall(P, scanner_detection(4, P), All), length(All, Len).

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
