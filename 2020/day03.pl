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
