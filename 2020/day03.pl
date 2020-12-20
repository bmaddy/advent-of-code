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
slope_line(_, []).
slope_line(_, [_|[]]).
slope_line([X,Y], [[X1,Y1], [X2,Y2]|Ps]) :-
    plus(X1, X, X2),
    plus(Y1, Y, Y2),
    slope_line([X,Y], [[X2,Y2]|Ps]).
