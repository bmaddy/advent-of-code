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

coord(x, X, [X|_]).
coord(y, Y, [_,Y]).

max_coord(Axis, Max, [P]) :-
    coord(Axis, Max, P).
max_coord(Axis, Max, [P|T]) :-
    coord(Axis, Max1, P),
    max_coord(Axis, Max2, T),
    Max is max(Max1, Max2).
%% ?- phrase_from_file(tree_coord(0, Cs), 'input03.txt'), max_coord(x, X, Cs).
%% Cs = [[1, 0], [2, 0], [16, 0], [17, 0], [24, 0], [30, 0], [1, 1], [3|...], [...|...]|...],
%% X = 30 ;
%% false.

%% ?- phrase_from_file(tree_coord(0, Cs), 'input03.txt'), max_coord(y, Y, Cs).
%% Cs = [[1, 0], [2, 0], [16, 0], [17, 0], [24, 0], [30, 0], [1, 1], [3|...], [...|...]|...],
%% Y = 322 ;
%% false.
