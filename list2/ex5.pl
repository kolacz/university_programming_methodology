%ex5

head(H,[H|_]).

last([X],X).
last([_|T],X) :- last(T,X).

tail(T,[_|T]).

init([_],[]).
init([H|T],[H|S]) :- init(T,S).

prefix([],[_|_]).
prefix([],[]).
prefix([H|T],[H|S]) :- prefix(T,S).

suffix(X,X).
suffix([_|T],L) :- suffix(T,L).
