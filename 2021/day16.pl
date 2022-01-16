:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

hex_bits(48, [0,0,0,0]).
hex_bits(49, [0,0,0,1]).
hex_bits(50, [0,0,1,0]).
hex_bits(51, [0,0,1,1]).
hex_bits(52, [0,1,0,0]).
hex_bits(53, [0,1,0,1]).
hex_bits(54, [0,1,1,0]).
hex_bits(55, [0,1,1,1]).
hex_bits(56, [1,0,0,0]).
hex_bits(57, [1,0,0,1]).
hex_bits(65, [1,0,1,0]).
hex_bits(66, [1,0,1,1]).
hex_bits(67, [1,1,0,0]).
hex_bits(68, [1,1,0,1]).
hex_bits(69, [1,1,1,0]).
hex_bits(70, [1,1,1,1]).

hex_binary([], []).
hex_binary([H|Hex], Binary) :-
    hex_bits(H, Bs),
    append(Bs, T, Binary),
    hex_binary(Hex, T).
%?- hex_binary(`0123456789ABCDEF`, Bs).

%% NOTE: would xinteger//1 help?

%?- format(atom(A), '~D', [1000000]).
%?- format(atom(A), '~D', ['10']).

%?- phrase(bits_number(N), [1,1,0], Rest).
%% bit()

%% bits([], []).
%% bits([H|T]) --> bit(H), bits(T).

%% version(V) --> bits(Bs), { length(Bs, 3),
%%                            bits_number(Bs, V) }.

%% header -->
%%     version(V),
%%     type_id(T).

%% packet -> header.
