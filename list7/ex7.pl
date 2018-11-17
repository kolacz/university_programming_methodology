%ex6

word --> [].
word --> ['a'],word,['b'].

word2 --> [].
word2 --> ['a'],!, word2 ,['b'].

word3(0) --> [].
word3(X) --> ['a'],word3(Y),{X is Y+1},['b'].

%ex7

:- if(current_prolog_flag(dialect,swi)). 
:- use_module(library(arithmetic)). 
:- arithmetic_function(!/2).
:- arithmetic_function(!/1).
:- arithmetic_function('!!'/2).
:- arithmetic_function('!!'/1).
:- endif.

:- op(150, yf, '!!').

'!!'(N) :- '!!'(N,_).

:- op(150, yf, !).

!(N) :- !(N,_).
!(0,1) :- !.
!(N,R) :- !(N,1,R).
!(0,A,A).
!(N,A,R) :- N1 is N-1, A1 is A*N, !(N1, A1, R), !.


'!!'(0,0):-!.
'!!'(1,1):-!.
'!!'(N,R) :- (0 is N mod 2), !, N1 is N-1, '!!'(N1,1,R).
'!!'(N,R) :- '!!'(N,1,R).
'!!'(1,A,A).
'!!'(N,A,R) :- N1 is N-2, A1 is A*N, '!!'(N1, A1, R), !.