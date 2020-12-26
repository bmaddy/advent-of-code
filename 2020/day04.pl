:- use_module(library(dcg/basics)).
:- use_module(library(assoc)).
:- use_module(library(pairs)).

%% ?- portray_text(true).
%% true.

test_input(`ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in`).

pair(K-Value) --> string_without(":", Key), ":", nonblanks(Value), { atom_string(K, Key) }.
%% test_input(T), phrase(pair(K, V), T, Rest).

pairs([K-V]) --> pair(K-V), ( "\n\n" | ("\n", end_of_string ) | end_of_string ), !.
pairs([K-V|Ps]) --> pair(K-V), ( " " | "\n" ), pairs(Ps).
%% test_input(T), phrase(pairs(Ps), T, Rest).

end_of_string([], []).

passports([]) --> ( "\n" | end_of_string ).
passports([P|Ps]) --> pairs(P), passports(Ps).
%% test_input(T), phrase(passports(Ps), T, Rest).

has_required_keys(Pairs) :-
    pairs_keys(Pairs, Ks),
    list_to_set(Ks, Keyset),
    subset([byr, iyr, eyr, hgt, hcl, ecl, pid], Keyset).

part_1_test(N) :-
    test_input(Input),
    phrase(passports(Ps), Input),
    include(has_required_keys, Ps, Valid),
    length(Valid, N).
%% ?- part_1_test(N).
%% N = 2 ;
%% false.

part_1(N) :-
    phrase_from_file(passports(Ps), 'input04.txt'),
    include(has_required_keys, Ps, Valid),
    length(Valid, N).
%% ?- part_1(N).
%% N = 213 ;
%% false.

valid_passport(P) :-
    has_required_keys(P),
    pairs_to_lists(P, L),
    phrase(valid_values, L).

pairs_to_lists([], []).
pairs_to_lists([A-B|Ps], [[A,B]|Ls]) :- pairs_to_lists(Ps, Ls).

valid_values([[K,V]|Ps]) --> [[K,V]], { valid_pair(K, V) }, !, valid_values(Ps).
valid_values([]) --> [].

valid_pair(ecl, V) :- ecl(V).
valid_pair(pid, V) :- pid(V).
valid_pair(eyr, V) :- eyr(V).

ecl(V) :- atom_string(A, V), member(A, [amb, blu, brn, gry, grn, hzl, oth]).
pid(V) :- length(V, 9), phrase(number(_), V, []).
eyr(V) :- length(V, 4), phrase(number(N), V, []), between(2020, 2030, N).

%% phrase(vv(V), [[ecl, `gry`], [pid, `860033327`], [eyr, `2020`]], Rest).


%% valid_values([V|Vs]) --> ( ecl(V) | pid(V) ), !, valid_values(Vs).
%% valid_values([]) --> [].


%% ecl(A) --> [[ecl,V]], { atom_string(A, V), member(A, [amb, blu, brn, gry, grn, hzl, oth]) }.
%% %% pid(N) --> [[pid, N]].
%% pid(N) --> [[pid, string(N)]].

%% list([])     --> [].
%% list([L|Ls]) --> [L], list(Ls).

%% concatenation([]) --> [].
%% concatenation([List|Lists]) -->
%%     list(List),
%%     concatenation(Lists).

%% flatten1([A, B|T]) --> [[A, B]], !, flatten1(T).
%% flatten1([]) --> [].

%% vv([V|Vs]) --> flatten1(V), vv(Vs).
%% vv([]) --> [].

%% vv(V) --> e(V).

%% %% e(V) --> [[_,V]].
%% e(V) --> [[_,V]], int(V).
%% p(V) --> V, _.

%% int(N) --> integer(N).

%% phrase(vv(V), [[ecl, "gry"], [pid, "860033327"], [eyr, "2020"]], Rest).
%% integer(N, `123`, Rest).

%% NEXT: try writing as normal prolog
%% !!! we aren't parsing a list, it's nested
%% "The most common use of DCGs is to parse some list of symbols"



%% Ps = [ecl-"gry", pid-"860033327", eyr-"2020", hcl-"#fffffd", byr-"1937", iyr-"2017", cid-"147", hgt-"183cm"], pairs_to_lists(Ps, Ls), phrase(valid_values(V), Ls, Rest).

%% valid_values2([]).
%% valid_values2([P|Ps]) :-
%%     ecl2(P)
%%     ;  pid(P),
%%     valid_values2(Ps).

%% ecl2([ecl, V]) :- atom_string(A, V), member(A, [amb, blu, brn, gry, grn, hzl, oth]).
%% pid([pid, V]) :- 

%% Ps = [ecl-"gry"], pairs_to_lists(Ps, Ls), valid_values2(Ls).
