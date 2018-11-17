%ex7

perm([],[]).
perm(L,[H|T]) :-
    select(H,L,S),
    perm(S,T).