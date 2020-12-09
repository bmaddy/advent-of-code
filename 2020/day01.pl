:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

newline --> [10].

end_of_input([], []).

separator --> ( newline | call(end_of_input) ).

integers([]) --> [].
integers([I|Is]) --> integer(I), separator, integers(Is).

%% phrase(integers([1,2,3,4]), Cs), string_codes(S, Cs).
%% string_codes("1\n2\n3\n4\n", Cs), phrase(integers(Is), Cs).
%% phrase_from_file(integers(Is), 'test.txt').

... --> [] | [_], ... .

two_items(A, B) --> ..., [A], ..., [B], ... .

two_item_product(Ints, A, B, Product) :-
    phrase(two_items(A, B), Ints),
    2020 is A + B,
    Product is A*B.

part_1_test(A, B, Product) :-
    string_codes("1721
979
366
299
675
1456", Cs),
    phrase(integers(Ints), Cs),
    two_item_product(Ints, A, B, Product).

part_1(A, B, Product) :-
    phrase_from_file(integers(Ints), 'day01.txt'),
    two_item_product(Ints, A, B, Product).

three_items(A, B, C) --> ..., [A], two_items(B, C).

three_item_product(Ints, A, B, C, Product) :-
    phrase(three_items(A, B, C), Ints),
    2020 is A + B + C,
    Product is A*B*C.

part_2_test(A, B, C, Product) :-
    string_codes("1721
979
366
299
675
1456", Cs),
    phrase(integers(Ints), Cs),
    three_item_product(Ints, A, B, C, Product).

part_2(A, B, C, Product) :-
    phrase_from_file(integers(Ints), 'day01.txt'),
    three_item_product(Ints, A, B, C, Product).
