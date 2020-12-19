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

%% column_trees(_, [], [], []).
column_trees(Col, [Col|Ts]) -->
    `#`,
    { succ(Col, Next_Col) },
    column_trees(Next_Col, Ts).
column_trees(Col, Ts) -->
    `.`,
    { succ(Col, Next_Col) },
    column_trees(Next_Col, Ts),
    !.
column_trees(Col, [Col]) --> `#`.
column_trees(_, []) --> `.`.
%?- phrase(column_trees(0, Ts), `..##.......`, Rest).
%% Ts = [2, 3]

%?- phrase(column_trees(0, Ts), `..##.......
%?- `, Rest).
%% Ts = [2, 3]

end_of_string([], []).

tree_coord(Row, [Cols]) -->
    column_trees(0, Cols),
    end_of_string.
tree_coord(Row, [Cols|Cs]) -->
    column_trees(0, Cols),
    "\n",
    { succ(Row, Next_Row) },
    tree_coord(Next_Row, Cs).
%?- phrase(tree_coord(0, Cs), `..##.......
%?- #...#...#..`, Rest).
%% Cs = [[2, 3], [0, 4, 8]]

%?- test_input(T), phrase(tree_coord(0, Cs), T, Rest).
%% Cs = [[2, 3], [0, 4, 8], [1, 6, 9], [2, 4, 8, 10], [1, 5, 6, 9], [2, 4, 5], [1, 3|...], [1|...], [...|...]|...],
