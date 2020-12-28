:- use_module(library(dcg/basics)).
:- use_module(library(assoc)).
:- use_module(library(pairs)).
:- use_module(library(crypto)).

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

pair(K-V) -->
    ecl(K-V)
    ; pid(K-V)
    ; eyr(K-V)
    ; hcl(K-V)
    ; byr(K-V)
    ; iyr(K-V)
    ; cid(K-V)
    ; hgt(K-V).
pair(K-V) --> key(K), nonblanks(V).
%% test_input(T), phrase(pair(KV), T, Rest).

pairs([P]) --> pair(P), ( "\n\n" | ("\n", end_of_string ) | end_of_string ), !.
pairs([P|Ps]) --> pair(P), ( " " | "\n" ), !, pairs(Ps).
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

valid_passport(P, Pairs, Rest) :-
    has_required_keys(P),
    %% pairs_to_lists(P, L),
    phrase(valid_values(Pairs), P, Rest).

%% ?- test_input(I), phrase(passports([P|_]), I), valid_passport(P, Pairs, Rest).

... --> [] | [_], ... .

key(K) --> string_without(":", S), ":", { atom_string(K, S) }.

byr(byr-V) --> key(byr), integer(V), { between(1920, 2002, V) }.
iyr(iyr-V) --> key(iyr), integer(V), { between(2010, 2020, V) }.
eyr(eyr-V) --> key(eyr), integer(V), { between(2020, 2030, V) }.
hgt(hgt-V) --> key(hgt), integer(V), "cm", { between(150, 193, V) }.
hgt(hgt-V) --> key(hgt), integer(V), "in", { between(59, 76, V) }.

hcl(hcl-V) --> key(hcl), "#", nonblanks(S), { catch(hex_bytes(S, V), _, false) }.
ecl(ecl-V) -->
    key(ecl), nonblanks(V),
    { atom_string(A, V), member(A, [amb, blu, brn, gry, grn, hzl, oth]) }.
pid(pid-V) --> key(pid), digits(V), { length(V, 9) }.
cid(cid-V) --> key(cid), nonblanks(V).

%% ?- phrase(pairs(A), `byr:2002 hgt:60in hgt:190cm hcl:#123abc ecl:brn pid:000000001`).
%% A = [byr-2002, hgt-60, hgt-190, hcl-[18, 58, 188], ecl-"brn", pid-"000000001"].
%% ?- phrase(( ..., pairs(A), ... ), `byr:2003 hgt:190in hgt:190 hcl:#123abz hcl:123abc ecl:wat pid:0123456789`).
%% false.
