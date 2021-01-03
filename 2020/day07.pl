:- use_module(library(dcg/basics)).

%% portray_text(true).

rules --> rule, "\n", rules, !.
rules --> rule, ( "\n" | [] ), end_of_string.

rule --> bag(Outer), " contain ", contents(Outer), ".".

bag(bag(Adj, Color)) --> nonblanks(Adj), whites, nonblanks(Color), " bag", ( "s" | [] ), !.

contents(Outer) --> item(Inner, N), ", ",
                    { assertz(contains(Outer, Inner, N)) },
                    !,
                    contents(Outer).
contents(Outer) --> item(Inner, N), { assertz(contains(Outer, Inner, N)) }.
contents(_) --> "no other bags".

item(B, N) --> integer(N), whites, bag(B).

end_of_string([], []).

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
