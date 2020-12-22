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
    phrase(valid_values, P).

valid_values(V) --> ecl(V).

ecl(A) --> [ecl-V], { atom_string(A, V), member(A, [amb, blu, brn, gry, grn, hzl, oth]) }.

%% Ps = [ecl-"gry", pid-"860033327", eyr-"2020", hcl-"#fffffd", byr-"1937", iyr-"2017", cid-"147", hgt-"183cm"], phrase(ecl, Ps, Rest).
