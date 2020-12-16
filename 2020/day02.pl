:- use_module(library(dcg/basics)).
:- use_module(library(pio)).

%% policy(Min, Max, L) --> integer(Min), "-", integer(Max), white, nonblank(L), ": ".
%% policy(Min, Max, L) --> integer(Min), `-`, integer(Max), white, nonblank(L), `: `.
policy(Min, Max, L) --> integer(Min), [-], integer(Max), white, nonblank(L), [: ].

a(X) --> integer(X), "foo".
b(X) --> integer(X), 'foo'.
c(X) --> integer(X), `foo`.
d(X) --> integer(X), [foo].

%% string_chars(S, C), phrase(policy(1, 3, `a`), C).
%% phrase(policy(Min, Max, L), "1-3 a: ").

/*
phrase(a(1), S). %% S = [49, f, o, o].
phrase(b(1), S). %% error
phrase(c(1), S). %% S = [49, 102, 111, 111].
phrase(d(1), S). %% S = [49, foo].

phrase(a(N), [49, 102, 111, 111]). %% false
phrase(a(N), [49, f, o, o]). %% N = 1
phrase(a(N), "1foo"). %% N = 1
phrase(a(N), '1foo'). %% error
phrase(a(N), `1foo`). %% false
phrase(a(N), [1foo]). %% error

phrase(b(N), [49, 102, 111, 111]). %% error
phrase(b(N), "1foo"). %% error
phrase(b(N), '1foo'). %% error
phrase(b(N), `1foo`). %% error
phrase(b(N), [1foo]). %% error

phrase(c(N), [49, 102, 111, 111]). %% N = 1
phrase(c(N), "1foo"). %% false
phrase(c(N), '1foo'). %% error
phrase(c(N), `1foo`). %% N = 1
phrase(c(N), [1foo]). %% error

phrase(d(N), [49, 102, 111, 111]). %% false
phrase(d(N), [49, foo]). %% error
phrase(d(N), "1foo"). %% false
phrase(d(N), '1foo'). %% error
phrase(d(N), `1foo`). %% false
phrase(d(N), [1foo]). %% error

*/

%% policy(Min, Max, L) --> integer(Min), "-", integer(Max), white, nonblank(L), ": ".
%% policy(Min, Max, L) --> integer(Min), `-`, integer(Max), white, nonblank(L), `: `.
%% policy(Min, Max, L) --> integer(Min), [-], integer(Max), white, nonblank(L), [: ].

%% string_chars(S, C), phrase(policy(1, 3, `a`), C).
%% phrase(policy(1, 3, `a`), C), string_chars(S, C).
%% string_chars(S, C), phrase(policy(1, 3, 97), C).
%% phrase(policy(1, 3, 97), C), string_chars(S, C).

%% phrase(policy(Min, Max, L), `1-3 a: `).

%% line(policy(Low, High, Letter), Password) -->  integer(Low), "-", integer(High), " ", nonblank(Letter), ": ", nonblanks(Password).
policy(Low, High, Letter) -->  integer(Low), "-", integer(High), " ", nonblank(Letter).

line(Low, High, Letter, Password) --> policy(Low, High, Letter), ": ", nonblanks(Password).

end_of_string([], []).

lines([[Low, High, Letter, Password]|Ls]) -->
    line(Low, High, Letter, Password),
    ( "\n" | call(end_of_string)),
    !,
    lines(Ls).
lines([]) --> [].

%?- portray_text(true).

/*

phrase( lines(Ls), `1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc`, Rest).

*/

%% ... --> [] | [_], ... .
