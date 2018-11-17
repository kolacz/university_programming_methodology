%ex1

length1(X,N) :- length1(X,X,[],0,N).
length1([],A,A,N,N).
length1([_|T],X,A,M,N) :-
    length1(T,X,[_|A],M1,N), M1 is (M+1),!.

len(L, N) :- len(L, 0, N).
len([], A, A).
len([_|T], A, N) :- var(N), B is A + 1, len(T, B, N).
len([_|T], A, N) :- integer(N), A < N, B is A + 1, len(T, B, N), !.
	
%ex2

connection(wroclaw,warszawa).
connection(wroclaw,krakow).
connection(wroclaw,szczecin).
connection(lublin,gliwice).
connection(gliwice,gniezno).
connection(lublin,wroclaw).
connection(wroclaw,lublin).
connection(szczecin,lublin).
connection(gniezno,wroclaw).
connection(warszawa,szczecin).
connection(gliwice,szczecin).

trip(C1,C2,T) :- trip(C1,C2,[C2],T).
trip(C,C,A,A).
trip(C1,C2,A,T) :-
    connection(P,C2),
    \+(member(P,A)),
    trip(C1,P,[P|A],T).

%ex3

bin([0]).
bin([1|X]) :- bin(X,[]).
bin(A,A).
bin(X,A) :-
    bin(X, [H|A]),
    (H=0; H=1).

rbin([0]).
rbin(X) :- rbin1(X).
rbin1([1]).
rbin1([H|T]) :- rbin1(T), (H=0; H=1).

%ex4


node/3.
%node(leaf, 3, node(leaf,5,leaf)).
%node(node(node(leaf,4,leaf),2,node(leaf,5,leaf)),1,node(node(leaf,6,leaf),3,node(node(leaf,8,node(leaf,9,leaf)),7,leaf))).
mirror(leaf,leaf).
mirror(node(X,A,Y),node(X1,A,Y1)) :- mirror(X,Y1), mirror(Y,X1).

flatten(T,S) :- flatten(T,[],S).
flatten(leaf,A,A).
flatten(node(L,V,R),A,S):-
    flatten(R,A,RS),
    flatten(L,[V|RS],S).

%ex5

node(node(node(leaf,1,leaf),3,node(leaf,4,leaf)),5,node(node(leaf,7,node(leaf,8,leaf)),10,node(leaf,12,leaf))).

insert(X,leaf,node(leaf,X,leaf)).
insert(X,node(L,V,R),node(L,V,RS)) :- (X > V),!, insert(X,R,RS).
insert(X,node(L,V,R),node(LS,V,R)) :- insert(X,L,LS).  

tree([],A,A).
tree([H|S],A,T) :- insert(H,A,A1), tree(S,A1,T).

treesort(L,R) :- tree(L,leaf,T), flatten(T,R).

%ex6

concat_number(D,N):-concat_number(D,N,0).
concat_number([],A,A).
concat_number([H|T],N,A):-
    (A1 is A*10+H),
    concat_number(T,N,A1).

solve(A,C,E,P,R,S,U) :-
    permutation([A,C,E,P,R,S,U|_], [0,1,2,3,4,5,6,7,8,9]),
    U \= 0, P \= 0,
    concat_number([U,S,A], N1),
    concat_number([U,S,S,R], N2),
    concat_number([P,E,A,C,E], N3),
    N3 is N1 + N2, !.

%ex7

revall(X,X) :- \+(is_list(X)),!.
revall(L,R) :- revall(L,[],R).
revall([],A,A).
revall([H|T],A,S) :- revall(H,HR), revall(T,[HR|A],S).