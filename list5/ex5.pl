%ex1

concat([],[]).
concat([H|T],X):-
    append(H,S,X),
    concat(T,S).

appn(L,Res) :- appn(L,[],Res).
appn([],Acc,Acc).
appn([H|T],Acc-H,Res) :-
    appn(T,Acc,Res).


flatten(L,Res):-
    flatten(L,A-A,Res).

flatten([],A-AT,A):- AT=[].

flatten([H|T], Acc-[H|L], Res) :-
    atomic(H),!,
    flatten(T, Acc-L, Res).

flatten([H|T], A-AT, Res):-
    flatten(H, A-AT, R),
    flatten(T, R-_, Res).




flatten1(List, Flattened):-
  flatten1(List, [], Flattened).

flatten1([], Flattened, Flattened).

flatten1([H|T], Acc, [H|Flattened]):-
  atomic(H), !,
  flatten1(T, Acc, Flattened).

flatten1([H|T], Acc, Flattened):-
  flatten1(T, Acc, TailFlattened),
  flatten1(H, TailFlattened, Flattened).


halve(List, Left, Right):-
  halve(List, List, Left, Right).

halve(Right, [], [], Right):-
  !.
halve(Right, [_], [], Right):-
  !.
halve([Head|Tail], [_,_|Bound], [Head|Left], Right):-
  halve(Tail, Bound, Left, Right).