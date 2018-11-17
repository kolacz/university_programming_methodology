%ex1
perm([],[]).
perm([H|T],R):-
    perm(T,S),
    select(H,R,S).

%ex2
filter([],[]).
filter([H|T],[H|S]):-
    (H>=0),!,filter(T,S).
filter([H|T],S):-
    (H<0),filter(T,S).

count([],[_|_],0).
count([],[],0).
count([H|T],L,N1):-
    select(H,L,S),count(T,S,N2),(N1 is N2+1).

exp(B,E,R):-exp(B,E,1,R).
exp(_,0,A,A).
exp(B,E,A,R):- A1 is A*B, E1 is E-1, exp(B,E1,A1,R),!.

    
%ex3
factorial(0,1):-!.
factorial(N,M):-factorial(N,M,1).
factorial(0,A,A).
factorial(N,M,A):-
    N1 is N-1,
    A1 is A*N,
    factorial(N1,M,A1),!.

concat_number(D,N):-concat_number(D,N,0).
concat_number([],A,A).
concat_number([H|T],N,A):-
    (A1 is A*10+H),
    concat_number(T,N,A1).

decimal(0,[0]):-!.
decimal(N,L) :- decimal(N,L,[]).
decimal(0,A,A).
decimal(N,L,A) :-
    H is N mod 10,
    N1 is N div 10,
    decimal(N1,L,[H|A]),!.

%psoty i swawole
decimal2(X,Y):-
    factorial(X,Z),
    decimal(Z,Y).

%ex4

select_min([X],X,[]).
select_min([H|T],X,[H|R]):-
    select_min(T,X,R), H > X, !.
select_min([H|T],H,T).

sel_sort([],[]).
sel_sort(L,[H|T]) :-
    select_min(L,H,S),
    sel_sort(S,T),!.

%ex5

insert([],X,[X]).
insert([H|T],X,[H|R]):-
    insert(T,X,R),
    X>H,!.
insert([H|T],X,[X|[H|T]]).

ins_sort(X,Y) :- ins_sort(X,[],Y).
ins_sort([],A,A).
ins_sort([H|T],A,R) :- insert(A,H,S), ins_sort(T,S,R).

%ex6

reverse1(X,Y) :- reverse1(X,[],Y,Y).
reverse1([],A,A,[]).
reverse1([H|T],A,Y,[_|S]):-
    reverse1(T,[H|A],Y,S),!.

%ex7

perm1(X,Y) :- var(X),!, perm(Y,X).
perm1(X,Y) :- perm(X,Y).
