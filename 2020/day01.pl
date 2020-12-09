:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

newline --> [10].

eof([], []).

separator --> ( newline | call(eof) ).

integers([]) --> [].
integers([I|Is]) --> integer(I), separator, integers(Is).

%% phrase_from_file(integers(Is), 'foo.txt').
%% phrase(integers([1,2,3,4]), Cs), string_codes(S, Cs).
%% string_codes("1\n2\n3\n4\n", Cs), phrase(integers(Is), Cs).

part_1(Is) :- phrase_from_file(integers(Is), 'foo.txt').
