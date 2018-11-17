%:- module(andrzej_kolacz, [solve/2]).

:- op(200, fx, ~).
:- op(500, xfy, v).

solve([],[]):-!.

solve(Clauses, Solution) :-
    variables(Clauses,Varlist),
    valuation(Varlist,Clauses,Solution).

%predykat tworzący listę zmiennych 
%zdaniowych występujących w klauzulach
variables(Clauses,List) :- variables(Clauses,[],List).

variables([],A,A).

variables([H|T],A,Varlist) :- 
    (H = ~L v C),
    \+(member(L,A)),!,
    variables([C|T],[L|A],Varlist).

variables([H|T],A,Varlist) :-
    (H = L v C),
    (L \= ~_),
    \+(member(L,A)),!,
    variables([C|T],[L|A],Varlist).

variables([H|T],A,Varlist) :-
    H = _ v C,!,
    variables([C|T],A,Varlist).


variables([~H|T],A,Varlist) :- 
    \+(member(H,A)),!,
    (H \= []),
    variables(T,[H|A],Varlist).

variables([H|T],A,Varlist) :- 
    (H \= ~_),
    (H \= []),
    \+(member(H,A)),!,
    variables(T,[H|A],Varlist).

variables([_|T],A,Varlist) :-  
    variables(T,A,Varlist).



%predykat sprawdzający czy zmienna zdaniowa
%występuje w klauzuli
vic(L,L):-!.
vic(L,L v _).
vic(V,_ v C):-
    vic(V,C).

% -//- negacja zmiennej zdaniowej -//-
vnic(L,~L):-!.
vnic(L,~L v _).
vnic(V, _ v C):-
    vnic(V,C).

%prezentacja starań i zasugerowanie 
%różnorodności rozpatrywanych podejść

%remove(L,L,~[]).
%remove(L,L v C,R):-
%    remove(L,C,R),!.
%remove(V,L v C1, L v C2):-
%    remove(V,C1,C2),!.
%remove(_,C,C).


%predykat który usuwa ze zbioru klauzul
%te, które przy rozpatrywanym wartościowaniu
%są prawdziwe
reduce(Var,Clauses,t,Res):- reduce(Var,Clauses,t,[],Res).
reduce(Var,Clauses,f,Res):- reduce(Var,Clauses,f,[],Res).
reduce(Var,Clauses,x,Res):- reduce(Var,Clauses,x,[],Res).

reduce(_,[],t,A,A).
reduce(_,[],f,A,A).
reduce(_,[],x,A,A).

reduce(V,[C|CS],t,A,Res):-
    vic(V,C),!,
    reduce(V,CS,t,A,Res).

reduce(V,[C|CS],t,A,Res):-
    reduce(V,CS,t,[C|A],Res).

reduce(V,[C|CS],f,A,Res):-
    vnic(V,C),!,
    reduce(V,CS,f,A,Res),!.

reduce(V,[C|CS],f,A,Res):-
    reduce(V,CS,f,[C|A],Res).

reduce(V,[C|CS],x,A,Res):-
    \+(vic(V,C)),
    \+(vnic(V,C)),
    reduce(V,CS,x,[C|A],Res).

%predykat tworzący wartościowanie
valuation(Varlist,Clauses,Valuation) :- valuation(Varlist,Clauses,[],Valuation).

%zwraca wartościowanie gdy "jesteśmy w stanie"
%opróżnić wejściową listę klauzul
valuation([],[],A,A).

valuation([V|VS],Clauses,A,Valuation):-
    reduce(V,Clauses,x,R),
    valuation(VS,R,[(V,x)|A],Valuation),!.

valuation([V|VS],Clauses,A,Valuation):-
    reduce(V,Clauses,f,R),
    valuation(VS,R,[(V,f)|A],Valuation).

valuation([V|VS],Clauses,A,Valuation):-
    reduce(V,Clauses,t,R),
    valuation(VS,R,[(V,t)|A],Valuation).