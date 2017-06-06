/*
    Authors:
    Shane Barrantes
    Ty Skelton
    Griffin Gonsalves
*/

/* Exercise 1 */

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

% a)
schedule(S,P,T) :- where(C,P), enroll(S,C), when(C,T).

% b)
usage(P,T)      :- when(C, T), where(C, P).

% c)
conflict(C1,C2) :- when(C1, T), where(C1, P), when(C2, T),
                   where(C2, P), C1 \= C2.

% d)
meet(S1, S2)    :- enroll(S1, C1), enroll(S2, C2), when(C1,T),
                   when(C2, T), (T=T-1; T=T; T=T+1).


/* Exercise 2 */

% a)
rdup([H|T],M)   :- member(H, T) -> rdup(T, M) ; rdup(T, M0), M = [H|M0].
rdup([],M)      :- M = [].

% b)
flat([H|T], F)  :- is_list(H) -> flat(H, F) ; flat(T, F0), F = [H|F0].
flat([], F)     :- F = [].

% c)
parse(_,[],V)    :- V = [].
parse(I,[H|T],V) :- I = 1 -> V = [H] ; I0 is I - 1, parse(I0, T, V).

project([H|T], P, L) :- parse(H, P, V), project(T, P, L0), append(V, L0, L).
project([], _, L) :- L = [].
