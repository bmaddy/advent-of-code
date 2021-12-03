:- use_module(library(dcg/basics)).
:- use_module(library(listing)).

%?- portray_text(true).

test_input(`199
200
208
210
200
207
240
269
260
263`).

end_of_string([], []).


%% depths([]) --> blanks, end_of_string.
%% depths([D|Ds]) --> integer(D), blanks, depths(Ds).

%?- test_input(Cs), phrase(depths(Ds), `200`, Rest).
%?- test_input(Cs), phrase(depths(Ds), `199\n200`, Rest).
%?- test_input(Cs), phrase(depths(Ds), `199\n200\n208`, Rest).

%% pairs([]) --> integer(_), blanks, end_of_string.
%% pairs([[A,B]]) --> integer(A), blanks,
%%                    integer(B), blanks.

%% pairs([]) --> [].

%% pairs([]) --> blanks, end_of_string.
%% pairs([]) --> integer(_), blanks, end_of_string.
%% pairs([[A,B]|Cs]) --> integer(A), blanks,
%%                       integer(B), blanks,
%%                       !,
%%                       pairs(Cs).

%?- listing(pairs).
%?- test_input(Cs), phrase(pairs(Ds), `200`, Rest).
%?- test_input(Cs), phrase(pairs(Ds), `199\n200`, Rest).
%?- test_input(Cs), phrase(pairs(Ds), `199\n200\n208`, Rest).
%?- test_input(Cs), phrase(pairs(Ds), `199\n200\n208\n210`, Rest).

%% depths() --> blanks, end_of_string.
%% depths([[D1,D2]|Ds]) --> integer(D1), blanks,
%%                          integer(D2), blanks,
%%                          depths([D2|Ds]).

%% depths --> integer(_), blanks, end_of_string.
%% depths([D1|D2]), [D2] --> integer(D1), blanks,
%%                         integer(D2),
%%                         { assert(pair(D1, D2)) },
%%                         depths.

%?- test_input(Cs), phrase(depths, `200`, Rest).
%?- test_input(Cs), phrase(depths(As), `199\n200`, Rest).
%?- pair(A,B).

%?- test_input(Cs), phrase(depths(A,B), `199\n200`, Rest).
%?- test_input(Cs), phrase(depths(A,B), Cs, Rest).


%% adjacent(D1,D2), [D2] --> integer(D1), blanks,
%%                           integer(D2).
%% pairs(_) --> blanks, end_of_string.
%% pairs([[A|B]|Ps]) --> adjacent(A, B), blanks, pairs(Ps).

%?- test_input(Cs), phrase(adjacent(A,B), `199\n200`, Rest).
%?- test_input(Cs), phrase(depths(A,B), Cs, Rest).

%% depths([]) --> end_of_string.
%% depths([D|Cs]) --> integer(D), blanks,
%%                    depths(Cs).

%% assert_pairs([],_).
%% assert_pairs(_,[]).
%% assert_pairs([A|As],[B|Bs]) :-
%%     assert(pair(A,B)),
%%     assert_pairs(As,Bs).

%?- assert_pairs([a,b,c], [b,c]).

%?- pair(A,B).
%?- listing(pair).
%?- assert_pairs([a,b,c], [g,g]), pair(A,B).

%% depths() --> { format("end\n") }, end_of_string.
%% depths() --> integer(D), blanks, { format("last-int(~w)\n", D) }.
%% depths(), [integer(D2)] --> integer(D1), blanks,
%%              integer(D2), blanks,
%%              { format("int_successor(~w, ~w)\n", [D1,D2]) }.

%?- hit(A).
%?- test_input(Cs), phrase(depths(), ``, Rest).
%?- test_input(Cs), phrase(depths(), `200`, Rest).
%?- test_input(Cs), phrase(depths(), `199\n200`, Rest).
%?- test_input(Cs), phrase(depths(), `199\n200\n208`, Rest).
%?- test_input(Cs), phrase(depths([D|Ds]), `199\n200\n208\n210`, Rest), assert_pairs([D|Ds], Ds).


depths([]) --> [].
depths([D|Ds]) --> integer(D), blanks, depths(Ds).
assert_successors([],_).
assert_successors(_,[]).
assert_successors([A|As],[B|Bs]) :-
    format("successor(~w,~w)\n", [A,B]),
    assert(successor(A,B)),
    assert_successors(As,Bs).


%?- successor(A,B).
%?- assert_successors([1,2,3], [2,3]).
%?- test_input(Cs), phrase(depths(Ds), ``, Rest), assert_successors([D|Ds], Ds).
%?- test_input(Cs), phrase(depths(Ds), `200`, Rest), assert_successors([D|Ds], Ds).
%?- test_input(Cs), phrase(depths(Ds), `199\n200`, Rest), assert_successors([D|Ds], Ds).
%?- test_input(Cs), phrase(depths(Ds), `199\n200\n208`, Rest), assert_successors([D|Ds], Ds).
%?- test_input(Cs), phrase(depths([D|Ds]), `199\n200\n208\n210`, Rest), assert_successors([D|Ds], Ds).
