test_input(`FBFBBFFRLR
BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL`).

mid(Low, High, Mid) :-
    plus(Low, Dist, High),
    divmod(Dist, 2, Half, 0),
    plus(Low, Half, Mid).

row(Min, Max, N) --> "F", { mid(Min, Max, Mid) }, row(Min, Mid, N), !.
row(Min, Max, N) --> "B", { mid(Min, Max, Mid) }, row(Mid, Max, N), !.
row(Min, _, Min) --> "F".
row(_, Max, N) --> "B", { plus(N, 1, Max) }.
%% ?- phrase(row(0, 128, N), `FBFBBFFRLR`, Rest).
%% N = 44,
%% Rest = "RLR".

col(Min, Max, N) --> "L", { mid(Min, Max, Mid) }, col(Min, Mid, N), !.
col(Min, Max, N) --> "R", { mid(Min, Max, Mid) }, col(Mid, Max, N), !.
col(Min, _, Min) --> "L".
col(_, Max, N) --> "R", { plus(N, 1, Max) }.
%% ?- phrase(col(0, 8, N), `RLR`, Rest).
%% N = 44,
%% Rest = "RLR".

seat(Row, Col, Id) --> row(0, 128, Row), col(0, 8, Col), { Id is 8*Row + Col }.
%% ?- phrase(seat(Row, Col, Id), `FBFBBFFRLR`, Rest).
%% Row = 44,
%% Col = 5,
%% Id = 357,
%% Rest = [].
%% ?- phrase(seat(Row, Col, Id), `BFFFBBFRRR`, Rest).
%% Row = 70,
%% Col = 7,
%% Id = 567,
%% Rest = [].
%% ?- phrase(seat(Row, Col, Id), `FFFBBBFRRR`, Rest).
%% Row = 14,
%% Col = 7,
%% Id = 119,
%% Rest = [].
%% ?- phrase(seat(Row, Col, Id), `BBFFBBFRLL`, Rest).
%% Row = 102,
%% Col = 4,
%% Id = 820,
%% Rest = [].

end_of_string([], []).

highest_seat(Id) --> seat(_, _, Id), ( ( "\n", end_of_string ) | end_of_string ).
highest_seat(Id) --> seat(_, _, A), "\n", highest_seat(B), !, { Id is max(A, B) }.
%% ?- test_input(I), phrase(highest_seat(Id), I, Rest).
%% I = "FBFBBFFRLR\nBFFFBBFRRR\nFFF...BFRLL",
%% Id = 820,
%% Rest = [].

part_1_test(Id) :-
    test_input(I),
    phrase(highest_seat(Id), I).
%% ?- part_1_test(Id).
%% Id = 820.

part_1(Id) :-
    phrase_from_file(highest_seat(Id), 'input05.txt').
%% ?- part_1(Id).
%% Id = 842.
