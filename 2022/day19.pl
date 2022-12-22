:- autoload(library(readutil), [ read_file_to_codes/3
                               ]).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- autoload(library(apply), [ maplist/4
                            ]).

% finite state machines reference
% https://homepage.cs.uri.edu/faculty/hamel/courses/2010/spring2010/csc481/lecture-notes/ln011a.pdf

replace_nth0(0, [_,B,C,D], A, [A,B,C,D]).
replace_nth0(1, [A,_,C,D], B, [A,B,C,D]).
replace_nth0(2, [A,B,_,D], C, [A,B,C,D]).
replace_nth0(3, [A,B,C,_], D, [A,B,C,D]).

bot_funds_index(0, 0).
bot_funds_index(1, 0).
bot_funds_index(2, 1).
bot_funds_index(3, 2).

buy([Bots0, Resources0], Costs, [Bots, Resources]) :-
    bot_funds_index(B, R),
    nth0(B, Costs, Cost),
    nth0(R, Resources0, Resource),
    Cost #=< Resource,
    nth0(B, Bots0, Bot),
    replace_nth0(B, Bots0, Bot + 1, Bots),
    replace_nth0(R, Resources0, Resource - Cost, Resources).
% buy([[1,1,1,1], [10,20,10,5]], [4,2,14,7], M1).

shop(A, _, A).
shop(Before, Blueprint, After) :-
    buy(Before, Blueprint, M),
    shop(M, Blueprint, After).
% shop([[1,0,0,0], [10,0,0,0]], [4,2,14,7], M1).

collect(Before, Blueprint, After).

build(Before, Blueprint, After).

process([R0, M0], Blueprint, [R,M]) :-
    shop([R0, M0], Blueprint, [R, Leftover]),
    maplist(plus, Leftover, R0, M).

/*
process([[1,0,0,0], [0,0,0,0]], [4,2,14,7], M1).
B = [4,2,14,7], process([[1,0,0,0], [0,0,0,0]], B, M1), process(M1, B, M2).
B = [4,2,14,7], process([[1,0,0,0], [0,0,0,0]], B, M1), process(M1, B, M2), process(M2, B, M3).
*/
