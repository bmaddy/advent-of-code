:- use_module(library(dcg/basics)).

%% portray_text(true).

rules --> rule, "\n", rules, !.
rules --> rule, ( "\n" | [] ), end_of_string.

rule --> bag(Outer), " contain ", contents(Outer), ".".

bag(bag(Adj, Color)) --> nonblanks(A), whites, nonblanks(C), " bag", ( "s" | [] ),
                         { atom_codes(Adj, A), atom_codes(Color, C) }, !.

contents(Outer) --> item(Inner, N), ", ",
                    { assertz(contains(Outer, Inner, N)) },
                    !,
                    contents(Outer).
contents(Outer) --> item(Inner, N), { assertz(contains(Outer, Inner, N)) }.
contents(_) --> "no other bags".

item(B, N) --> integer(N), whites, bag(B).

end_of_string([], []).

path(A, B) :- contains(A, B, _).
path(A, C) :- contains(A, B, _), path(B, C).
%% path(A, B) :- path(A, B, []).
%% path(A, B, [A]) :- contains(A, B, _).
%% path(A, C, Visited) :- \+ member(A, Visited), contains(A, B, _), path(B, C, [B|Visited]).

reload_input :-
    abolish(contains/3),
    phrase_from_file(rules, 'input07.txt', []).


%% tests and results

part_1(N) :-
    reload_input,
    setof(A, path(A, bag(shiny, gold)), As),
    length(As, N).

part_1_test(N) :-
    reload_test(_),
    setof(A, path(A, bag(shiny, gold)), As),
    length(As, N).

reload_test(Rest) :-
    abolish(contains/3),
    test_input(I), phrase(rules, I, Rest).
%% ?- reload_test(R).
%% R = [].
%% ?- contains(A, B, 6).
%% A = bag("vibrant", "plum"),
%% B = bag("dotted", "black").

test_input(`light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.`).
