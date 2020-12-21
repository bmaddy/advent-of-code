test_input(`..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#`).
test_row(`..##.......`).

column_trees(X, Y, [[X, Y]|Ts]) -->
    `#`,
    { succ(X, Next_X) },
    column_trees(Next_X, Y, Ts).
column_trees(X, Y, Ts) -->
    `.`,
    { succ(X, Next_X) },
    column_trees(Next_X, Y, Ts),
    !.
column_trees(X, Y, [[X, Y]]) --> `#`.
column_trees(_, _, []) --> `.`.
%?- phrase(column_trees(0, 0, Ts), `..##.......`, Rest).
%% Ts = [[0, 2], [0, 3]]


%?- phrase(column_trees(0, 0, Ts), `..##.......
%?- `, Rest).
%% Ts = [[0, 2], [0, 3]]

end_of_string([], []).

tree_coord(Y, Cs) -->
    column_trees(0, Y, Cs),
    ( "\n" | end_of_string ).
tree_coord(Y, Cs) -->
    column_trees(0, Y, A),
    "\n",
    { succ(Y, Next_Y) },
    tree_coord(Next_Y, B),
    { append(A, B, Cs)}.
%?- phrase(tree_coord(0, Cs), `..##.......`, Rest).
%% Cs = [[0, 2], [0, 3]]

%?- phrase(tree_coord(0, Cs), `..##.......
%?- #...#...#..`, Rest).
%% Cs = [[0, 2], [0, 3], [1, 0], [1, 4], [1, 8]],

%?- test_input(T), phrase(tree_coord(0, Cs), T, Rest).

%?- phrase_from_file(tree_coord(0, Cs), 'input03.txt').

max_xy([X,Y], [[X,Y]]).
max_xy([X,Y], [[X1,Y1]|T]) :-
    max_xy([X2,Y2], T),
    X is max(X1, X2),
    Y is max(Y1, Y2).
%% phrase_from_file(tree_coord(0, Cs), 'input03.txt'), max_xy([X,Y], Cs).
%% Cs = [[1, 0], [2, 0], [16, 0], [17, 0], [24, 0], [30, 0], [1, 1], [3|...], [...|...]|...],
%% X = 30,
%% Y = 322 ;
%% false.

%% a slope describes a line when subsequent points are [X,Y] away from previous points.
slope_wrapped_line(_, _, []).
slope_wrapped_line(_, _, [_|[]]).
slope_wrapped_line([X,Y], Width, [[X1,Y1], [Wrapped_X,Y2]|Ps]) :-
    plus(X1, X, X2),
    Wrapped_X is X2 mod Width,
    plus(Y1, Y, Y2),
    slope_wrapped_line([X,Y], Width, [[Wrapped_X,Y2]|Ps]).
%?- length(S, 5), slope_wrapped_line([3,1], [[0,0]|S]).

keep_list([], _, []).
keep_list([H|T], Good, [H|Filtered]) :- member(H, Good), !, keep_list(T, Good, Filtered).
keep_list([_|T], Good, Filtered) :- keep_list(T, Good, Filtered).

/*
test_input(Input),
phrase(tree_coord(0, Trees), Input),
%% Technically this should be based off the input string, but I have trees in
%% the last column and final row, so this works for my input
max_xy([Max_X,Max_Y], Trees),
divmod(Max_Y, 1, Step_Count, _),
length(Steps, Step_Count),
succ(Max_X, Width),
slope_wrapped_line([3,1], Width, [[0,0]|Steps]),
keep_list(Steps, Trees, Hits).

expected Hits = [[6,2], [1, 4], [4, 5], [10, 7], [2, 8], [5, 9], [8, 10]]
*/

path_hits(Trees, [DX,DY], Hit_Count) :-
    %% Finds the largest available X and Y coordinates. Technically, this should
    %% be based off the input string, but I have trees in the last column and
    %% final row, so this works for my input.
    max_xy([Max_X,Max_Y], Trees),
    divmod(Max_Y, DY, Step_Count, _),
    length(Steps, Step_Count),
    %% Width is one more than the largest possible X position.
    succ(Max_X, Width),
    slope_wrapped_line([DX,DY], Width, [[0,0]|Steps]),
    keep_list(Steps, Trees, Hits),
    length(Hits, Hit_Count).

part_1_test(N) :-
    test_input(Input),
    phrase(tree_coord(0, Trees), Input),
    path_hits(Trees, [3,1], N).
%% part_1_test(N).
%% N = 7 ;
%% false.

part_1(N) :-
    phrase_from_file(tree_coord(0, Trees), 'input03.txt'),
    path_hits(Trees, [3,1], N).
%% ?- part_1(N).
%% N = 176 ;

many_slopes_product(Trees, Product) :-
    many_slopes_product(Trees, [[1,1], [3,1], [5,1], [7,1], [1,2]], Product).
many_slopes_product(_, [], 1).
many_slopes_product(Trees, [Slope|Ss], Product) :-
    path_hits(Trees, Slope, N),
    many_slopes_product(Trees, Ss, Rest),
    Product is N*Rest.

part_2_test(N) :-
    test_input(Input),
    phrase(tree_coord(0, Trees), Input),
    many_slopes_product(Trees, N).
%% ?- part_2_test(N).
%% N = 336 ;
%% false.

part_2(N) :-
    phrase_from_file(tree_coord(0, Trees), 'input03.txt'),
    many_slopes_product(Trees, N).
%% ?- part_2(N).
%% N = 5872458240 ;
%% false.
