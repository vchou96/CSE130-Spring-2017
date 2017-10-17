%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helpers

%isin(X,L) is true if X appears in L
isin(X,[X|_]).
isin(X,[_|T]) :- isin(X,T).

%hint: use append, reverse, bagof judiciously.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 1: Rules

%zip(L1,L2,L3) :- L1=[X1|XRest],
%		 L2=[Y1|YRest],
%		 L3= [X1,Y1|ZRest],
%		 zip([],[],[]).
%		 zip(XRest,YRest,ZRest).


zip([],[],[]).
%zip([XS|XRest],[YS|YRest],L3) :- zip(XRest,YRest,ZRest).
zip([XS|XRest],Y,[XS|L3Rest]) :-
	%X=[XS|XRest],
	%Y=[YS|YRest],	
	%isin(XS,L3),
	%delete(XS,L3,R),
	zip(XRest,Y,L3Rest).
zip(X,[YS|YRest],[YS|L3Rest]) :-
	%X=[XS|XRest],
	%Y=[YS|YRest],	
	%isin(YS,L3),
	%delete(YS,L3,R),
	zip(X,YRest,L3Rest).

assoc([[X,Y]|_],X,Y).
assoc([[H,T]|LRest],X,Y) :-
	assoc(LRest,X,Y).

		 . 
delete(X,[],[]).
delete(H,[H|T],R) :- delete(H,T,R).
delete(X,[H|T],[H|R]) :- delete(X,T,R).

checkUnique(X,[]).
checkUnique(X,Tail) :- not(isin(X,Tail)).

removeHelp([],[],_).
removeHelp([X|L1Rest],[X|L2Rest], SeenBefore) :-
		delete(X,L1Rest,R),
		checkUnique(X,SeenBefore),
		removeHelp(R,L2Rest,[X|SeenBefore]).

remove_duplicates([],[]).
remove_duplicates(L1,L2) :-
		removeHelp(L1,L2,[]).

union(L1,L2,L3) :- 
		append(L1,L2,COMBINE),
		remove_duplicates(COMBINE,L3).



inter([],_,List,List).
inter([X|L1Rest],L2,L3,List):-
	isin(X,L2),
	delete(X,L1Rest,R),
	inter(R,L2,L3,[X|List]),!.
inter([X|L1Rest],L2,L3,List):-
	inter(L1Rest,L2,L3,List).


intersection(_,[],[]). 
intersection([],_,[]). 
intersection(L1,L2,L3) :- 
	inter(L1,L2,L3,[]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 2: Facts

cost(carne_asada,3).
cost(lengua,2).
cost(birria,2).
cost(carnitas,2).
cost(adobado,2).
cost(al_pastor,2).
cost(guacamole,1).
cost(rice,1).
cost(beans,1).
cost(salsa,1).
cost(cheese,1).
cost(sour_cream,1).
cost(taco,1).
cost(tortilla,1).


ingredients(carnitas_taco, [taco,carnitas, salsa, guacamole]).
ingredients(birria_taco, [taco,birria, salsa, guacamole]).
ingredients(al_pastor_taco, [taco,al_pastor, salsa, guacamole, cheese]).
ingredients(guacamole_taco, [taco,guacamole, salsa,sour_cream]).
ingredients(al_pastor_burrito, [tortilla,al_pastor, salsa]).
ingredients(carne_asada_burrito, [tortilla,carne_asada, guacamole, rice, beans]).
ingredients(adobado_burrito, [tortilla,adobado, guacamole, rice, beans]).
ingredients(carnitas_sopa, [sopa,carnitas, guacamole, salsa,sour_cream]).
ingredients(lengua_sopa, [sopa,lengua,beans,sour_cream]).
ingredients(combo_plate, [al_pastor, carne_asada,rice, tortilla, beans, salsa, guacamole, cheese]).
ingredients(adobado_plate, [adobado, guacamole, rice, tortilla, beans, cheese]).

taqueria(el_cuervo, [ana,juan,maria], 
        [carnitas_taco, combo_plate, al_pastor_taco, carne_asada_burrito]).

taqueria(la_posta, 
        [victor,maria,carla], [birria_taco, adobado_burrito, carnitas_sopa, combo_plate, adobado_plate]).

taqueria(robertos, [hector,carlos,miguel],
        [adobado_plate, guacamole_taco, al_pastor_burrito, carnitas_taco, carne_asada_burrito]).

taqueria(la_milpas_quatros, [jiminez, martin, antonio, miguel],  
        [lengua_sopa, adobado_plate, combo_plate]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem 2: Rules

available_at(X,L) :- 
	taqueria(L,_,Z),
	isin(X,Z).

bagLength([],Length):-
	Length is 0.
bagLength([X|BagRest],Length) :-
	bagLength(BagRest,L),
	Length is L+1.
	
multi_available(X) :- 
	bagof(L,available_at(X,L),PLACES),
	bagLength(PLACES,LENGTH),
	LENGTH > 1.

works_at(X,L) :-
	taqueria(L,Z,_),
	isin(X,Z).

overworked(X) :-
	bagof(L,works_at(X,L),PEOPLE),
	bagLength(PEOPLE,LENGTH), 
	LENGTH > 1. 


calculateCost([],Cost):-
	Cost is 0.
calculateCost([X|XRest],Cost):-
	calculateCost(XRest,C),
	cost(X,Price),
	Cost is Price + C.

total_cost(X,K) :-
	ingredients(X,Ingred), 
	calculateCost(Ingred,Cost),
	K is Cost.


listCheck(L1,[X|L2Rest]):-
	isin(X,L1),
	listCheck(L1,L2Rest).
listCheck(_,[]).

has_ingredients(_,[]).
has_ingredients(X,Is) :- 
	ingredients(X,Ingred),
	listCheck(Ingred,Is).

avoid(L1,[X|L2Rest]):-
	not(isin(X,L1)),
	avoid(L1,L2Rest).
avoid(_,[]).

avoids_ingredients(X,Is) :-  
	ingredients(X,Ingred),
	avoid(Ingred,Is).



p1(L,X) :-  
	bagof(Z,has_ingredients(Z,X),L).

p2(L,Y) :-  
	bagof(Z,avoids_ingredients(Z,Y),L).

find_items(L,X,Y) :- p1(L1,X), p2(L2,Y),intersection(L1,L2,L).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
