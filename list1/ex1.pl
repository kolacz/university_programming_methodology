%ex1

my_cat.
me.

bird(_):-!.
earthworm(_):-!.
fish(_):-!.

cat(my_cat).
friends(me, my_cat).

likes(X,Y) :- bird(X), earthworm(Y).
likes(X,Y) :- cat(X), fish(Y).
likes(X,Y) :- friends(X,Y).
likes(X,Y) :- friends(Y,X). 
eats(my_cat,X) :- likes(my_cat, X).

%ex2
%brakuje: smok to zwierze, 

mily(X):-
	czlowiek(X),
	odwiedzazoo(X).

stykasie(X, Y):-
	mieszkawzoo(X),
	zwierze(X),
	czlowiek(Y),
	odwiedzazoo(Y).

szczesliwe(X):-
	stykasie(X,Y),
	czlowiek(Y),
	mily(Y).

nie_smok(X):-
	szczesliwe(X),
	mieszkawzoo(X).

zwierze(X):-
	smok(X).

czlowiek(john).
odwiedzazoo(john).
smok(onufry).
mieszkawzoo(reksio).
zwierze(reksio).


%ex3

sokrates.
human(sokrates).
mortal(X):-human(X).

%ex4;ex5

male(adam).
male(john).
male(mark).
male(joshua).
male(david).

female(eve).
female(helen).
female(ivonne).
female(anna).

parent(adam,helen).
parent(adam,ivonne).
parent(adam,anna).
parent(eve,helen).
parent(eve,ivonne).
parent(eve,anna).
parent(john,joshua).
parent(helen,joshua).
parent(ivonne,david).
parent(mark,david).
sibling(X,Y):- parent(Z,X), parent(Z,Y), female(Z), \+(X==Y).
sister(X,Y):- sibling(X,Y), female(X).
grandson(X,Y):- parent(Z,X), parent(Y,Z).
cousin(X,Y):- parent(A,X), parent(B,Y), sibling(A,B).
descendant(X,Y):- parent(Y,X).
descendant(X,Y):- parent(Z,X), descendant(Z,Y).
is_mother(X):- parent(X,_), female(X).
is_father(X):- parent(X,_), male(X).

%ex6

direct_connection(wroclaw,warszawa).
direct_connection(wroclaw,krakow).
direct_connection(wroclaw,szczecin).
direct_connection(szczecin,lublin).
direct_connection(szczecin,gniezno).
direct_connection(warszawa,katowice).
direct_connection(gniezno,gliwice).
direct_connection(lublin,gliwice).

% predykat "connection" zapętla się, gdy są połączenia z A do B i z B do A, np:
direct_connection(gliwice,wroclaw).

otc(X,Y) :- direct_connection(X,Z), direct_connection(Z,Y).
ttc(X,Y) :- otc(X,Z), direct_connection(Z,Y).

connection(X,Y) :- direct_connection(X,Y).
connection(X,Y) :- direct_connection(X,Z), connection(Z,Y).

%ex7

pierwszy(Dom,uklad(Dom,_,_,_,_)).
drugi(Dom,uklad(_,Dom,_,_,_)).
trzeci(Dom,uklad(_,_,Dom,_,_)).
czwarty(Dom,uklad(_,_,_,Dom,_)).
piaty(Dom,uklad(_,_,_,_,Dom)).
 
stoi(Dom,Uklad) :-
    pierwszy(Dom,Uklad);
    drugi(Dom,Uklad);
    trzeci(Dom,Uklad);
    czwarty(Dom,Uklad);
    piaty(Dom,Uklad).
 
sasiad(X,Y,Uklad) :-
    pierwszy(X,Uklad), drugi(Y,Uklad) ;
    drugi(X,Uklad), trzeci(Y,Uklad) ;
    trzeci(X,Uklad), czwarty(Y,Uklad) ;
    czwarty(X,Uklad), piaty(Y,Uklad) ;
    pierwszy(Y,Uklad), drugi(X,Uklad) ;
    drugi(Y,Uklad), trzeci(X,Uklad) ;
    trzeci(Y,Uklad), czwarty(X,Uklad) ;
    czwarty(Y,Uklad), piaty(X,Uklad).
 
dobry(U) :- 
    pierwszy((_,norweg,_,_,_), U),
    trzeci((_,_,_,_,mleko), U),
    stoi((czerwony,anglik,_,_,_), U),
    stoi((_,hiszpan,pies,_,_), U),
    stoi((zielony,_,_,_,kawa), U),
    stoi((_,ukrainiec,_,_,herbata), U),
    stoi((_,_,waz,winston,_), U),
    stoi((zolty,_,_,koole,_), U),
    stoi((_,_,_,lucky,sok), U),
    stoi((_,japonczyk,_,kenty,_),U),
    stoi((_,_,_,_,wodka),U),
    stoi((_,_,slon,_,_),U),
    sasiad((niebieski,_,_,_,_), (_,norweg,_,_,_), U),
    sasiad((_,_,_,chester,_), (_,_,lis,_,_), U),
    sasiad((_,_,_,koole,_), (_,_,kon,_,_), U),
    sasiad((zielony,_,_,_,_), (bialy,_,_,_,_), U).
    