:- use_module(library(dcg/basics)).

%% portray_text(true).

rules --> rule, "\n", rules, !.
rules --> rule, ( "\n" | [] ), end_of_string.

rule --> bag(Outer), " contain ", contents(Outer), ".".

bag(bag(Adj, Color)) --> nonblanks(A), whites, nonblanks(C), " bag", ( "s" | [] ),
                         { atom_codes(Adj, A), atom_codes(Color, C) }, !.

contents(Outer) --> item(Inner-N), ", ",
                    { assertz(contains(Outer, Inner-N)) },
                    !,
                    contents(Outer).
contents(Outer) --> item(Inner-N), { assertz(contains(Outer, Inner-N)) }.
contents(_) --> "no other bags".

item(B-N) --> integer(N), whites, bag(B).

end_of_string([], []).

path(A, B) :- contains(A, B-_).
path(A, C) :- contains(A, B-_), path(B, C).
%% path(A, B) :- path(A, B, []).
%% path(A, B, [A]) :- contains(A, B, _).
%% path(A, C, Visited) :- \+ member(A, Visited), contains(A, B, _), path(B, C, [B|Visited]).

reload_input :-
    abolish(contains/2),
    phrase_from_file(rules, 'input07.txt', []).

%% grouped_contains(P, [C, N]) :- contains(P, C, N).

bag_contents(B, Cs) :- setof(C, contains(B, C), Cs).

bag_count(B, 1) :- \+ contains(B, _).
%% bag_count(B, N) :- setof(B, contains(B, C-N), .
%% WORKING HERE: there must be a way to return all counts and what to multiply them by...
%% could we do bottom up somehow?
bag_count(P, N) :- contains(P, C-Mult), bag_count(C, Desc_count), { N is Mult*Desc_count }.



%% tests and results

part_1(N) :-
    reload_input,
    setof(A, path(A, bag(shiny, gold)), As),
    length(As, N).
%% ?- time(part_1(N)).
%% % 595,270 inferences, 12.305 CPU in 12.350 seconds (100% CPU, 48376 Lips)
%% N = 252.

part_1_test(N) :-
    reload_test(_),
    setof(A, path(A, bag(shiny, gold)), As),
    length(As, N).
%% ?- time(part_1_test(N)).
%% % 1,618 inferences, 0.001 CPU in 0.002 seconds (41% CPU, 2436747 Lips)
%% N = 4.

reload_test :- reload_test(_).
reload_test(Rest) :-
    abolish(contains/2),
    test_input(I),
    phrase(rules, I, Rest).
%% ?- reload_test(R).
%% R = [].
%% ?- contains(A, B-6).
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
