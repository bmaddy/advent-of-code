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
    { succ(Y, Next_Y) },
    column_trees(X, Next_Y, Ts).
column_trees(X, Y, Ts) -->
    `.`,
    { succ(Y, Next_Y) },
    column_trees(X, Next_Y, Ts),
    !.
column_trees(X, Y, [[X, Y]]) --> `#`.
column_trees(_, _, []) --> `.`.
%?- phrase(column_trees(0, 0, Ts), `..##.......`, Rest).
%% Ts = [[0, 2], [0, 3]]


%?- phrase(column_trees(0, 0, Ts), `..##.......
%?- `, Rest).
%% Ts = [[0, 2], [0, 3]]

end_of_string([], []).

tree_coord(X, Cs) -->
    column_trees(X, 0, Cs),
    ( "\n" | end_of_string ).
tree_coord(X, Cs) -->
    column_trees(X, 0, A),
    "\n",
    { succ(X, Next_X) },
    tree_coord(Next_X, B),
    { append(A, B, Cs)}.
%?- phrase(tree_coord(0, Cs), `..##.......`, Rest).
%% Cs = [[0, 2], [0, 3]]

%?- phrase(tree_coord(0, Cs), `..##.......
%?- #...#...#..`, Rest).
%% Cs = [[0, 2], [0, 3], [1, 0], [1, 4], [1, 8]],

%?- test_input(T), phrase(tree_coord(0, Cs), T, Rest).

%% Fixme
%?- phrase_from_file(tree_coord(0, Cs), 'input03.txt').
