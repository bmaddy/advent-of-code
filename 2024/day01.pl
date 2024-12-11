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

number([D|Ds]) --> digit(D), number(Ds).
number([D])    --> digit(D).

integer(N) --> number(Ds), !, ws, { number_chars(N, Ds) }.

integers([N|Ns]) --> integer(N), ws, integers(Ns).
integers([N])    --> integer(N).

ws --> [W], { char_type(W, whitespace) }, !, ws.
ws --> [].



%% lines([]) --> eos, !.
%% lines([L|Ls]) --> line(L), lines(Ls).

%% line([]) --> ( "\n" | eos ), !.
%% line([C|Cs]) --> [C], line(Cs).

%% eos([], []).



%% nl --> "\n".
%% nl --> [].

%% line(Cs) -->
%%     [C],
%%     !,
%%     ( { C \== [] } ->
%%       line(T),
%%       { Cs = [C | T] }
%%     ; nl,
%%       { Cs = [] }).

%% lines(Ls) -->
%%     line(L),
%%     !,
%%     ( { L \== [[]] } ->
%%       lines(T),
%%       { Ls = [L | T] }
%%     ; { Ls = [] }).



line([C|Cs]) --> [C], { C \= '\n' }, line(Cs), !.
line([C]) --> [C], { C \= '\n' }.
%% line([]) --> "\n", !.
%% line([]) --> [].
%% ?- phrase(line(L), [], T).
%% ?- phrase(line(L), "", T).
%% ?- phrase(line(L), "a", T).
%% ?- phrase(line(L), "aa", T).
%% ?- phrase(line(L), "aa\n", T).
%% ?- phrase(line(L), "aa\nbb", T).

eos([], []).

%% lines([]) --> [].
lines([]) --> eos.
%% lines([]) --> "\n".
lines([L]) --> line(L), eos.
lines([L|Ls]) --> line(L), "\n", lines(Ls).
%% lines([L]) --> line(L), [].
%% ?- phrase(lines(L), "", T).
%% ?- phrase(lines(L), "aa", T).
%% ?- phrase(lines(L), "aa\n", T).
%% ?- phrase(lines(L), "aa\nbb", T).
%% ?- phrase(lines(L), "aa\nbb\ncc", T).


%% ?- char_type('\n', S).
%% ?- char_type(C, control).
%% ?- char_type(' ', S), \+ char_type('\n', S).

%% ?- char_type('\n', S), \+ char_type(' ', S).



%% ?- phrase(number(X), "111 222", T).
%% ?- phrase(integers(X), "111 222\n333 444", T).

%% digits(Ds) -->

test_input("3   4\n4   3\n2   5\n1   3\n3   9\n3   3").

%% ?- test_input(I).
