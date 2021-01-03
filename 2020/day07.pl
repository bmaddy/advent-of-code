:- use_module(library(dcg/basics)).

%% portray_text(true).

rules([H|T]) --> rule(H), "\n", rules(T), !.
rules([R]) --> rule(R), ( "\n" | [] ), end_of_string.

end_of_string([], []).

rule(rule(B, C)) --> bag(B), " contain ", contents(C), ".".

bag(bag(Adj, Color)) --> nonblanks(Adj), whites, nonblanks(Color), " bag", ( "s" | [] ), !.

contents([H|T]) --> item(H), ", ", contents(T), !.
contents([I]) --> item(I).
contents([]) --> "no other bags".

item(N-B) --> integer(N), whites, bag(B).

test_input(`light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.`).
