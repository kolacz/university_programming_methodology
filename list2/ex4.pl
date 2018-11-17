%ex4

even([]).
even([_|[_|T]]) :- even(T).

append([],X,X).
append([H|T],X,[H|Y]) :-
    append(T,X,Y).

reverse(X,Y) :- reverse(X,[],Y).

reverse([],A,A).
reverse([H|T],A,X) :-
    reverse(T,[H|A],X).
    
palindrom(X) :- reverse(X,X).    
    
singleton([_]).