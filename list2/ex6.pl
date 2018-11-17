%ex6

sublist([],[]) :- !.
sublist([_|_],[]) :- !.
sublist([H|T],[H|S]) :- sublist(T,S).
sublist([_|T], L) :- sublist(T,L).