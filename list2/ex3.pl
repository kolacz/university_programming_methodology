%ex3
append([],X,X).
append([H|T],X,[H|Y]) :-
    append(T,X,Y).

select(H,[H|T],T).
select(X,[H|T],[H|S]) :-
    select(X,T,S).

append(X,X,Y).
select(X,[a,b,c,d,e],[a,c,d]).
append([a,b,c],X,[a,b,c,d,e]).
