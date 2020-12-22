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

%% ... --> [] | [_], ... .

pair(K-Value) --> string_without(":", Key), ":", nonblanks(Value), { atom_string(K, Key) }.
%% test_input(T), phrase(pair(K, V), T, Rest).

%% pairs([K-V]) --> pair(K-V), ( "\n\n" | end_of_string ), !.
pairs([K-V]) --> pair(K-V), ( "\n\n" | ("\n", end_of_string ) | end_of_string ), !.
pairs([K-V|Ps]) --> pair(K-V), ( " " | "\n" ), pairs(Ps).
%% test_input(T), phrase(pairs(Ps), T, Rest).

valid_passport(Pairs) :-
    pairs_keys(Pairs, Ks),
    list_to_set(Ks, Keyset),
    subset([byr, iyr, eyr, hgt, hcl, ecl, pid], Keyset).

end_of_string([], []).

passports([]) --> ( "\n" | end_of_string ).
passports([P|Ps]) --> pairs(P), { valid_passport(P) }, !, passports(Ps).
passports(Ps) --> pairs(_), passports(Ps).
%% passports([], []).
%% passports([P,P2|Ps]) --> pairs(P), pairs(P2), remainder(Ps).
%% test_input(T), phrase(passports(Ps), T, Rest).

part_1_test(N) :-
    test_input(Input),
    phrase(passports(Ps), Input),
    length(Ps, N).
%% ?- part_1_test(N).
%% N = 2 ;
%% false.

part_1(N) :-
    phrase_from_file(passports(Ps), 'input04.txt'),
    length(Ps, N).
