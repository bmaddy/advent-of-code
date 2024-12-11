:- use_module(library(charsio)).
:- use_module(library(dcgs)).
:- use_module(library(pio), [ phrase_from_file/2
                          ]).
:- use_module(library(lists)).

%% https://www.scryer.pl/csv
%% https://www.scryer.pl/builtins.html#open/3

%% Example:
%% https://github.com/mthom/scryer-prolog/blob/ea02b54e7fc164f026e4f52702088d5b0146c2a3/src/lib/simplex.pl#L1310

digit(D) --> [D], { char_type(D, decimal_digit) }.

number([D|Ds]) --> digit(D), number(Ds), !.
number([D])    --> digit(D).

integer(N) --> number(Ds), { number_chars(N, Ds) }.

integers([N|Ns]) --> integer(N), ws, integers(Ns), !.
integers([N])    --> integer(N).

ws --> " ", ws, !.
ws --> " ".
%% ?- phrase(ws, "  ", T).

eos([], []).

lines([]) --> eos.
lines([L]) --> integers(L), eos, !.
lines([L|Ls]) --> integers(L), "\n", lines(Ls).
%% ?- phrase(lines(L), "11 22  33\n44 55  66", T).

test_input("3   4\n4   3\n2   5\n1   3\n3   9\n3   3").
%% ?- test_input(I), phrase(lines(L), I, S).

