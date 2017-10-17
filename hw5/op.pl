sorted([]).
sorted([_]):-
	write(hi).
sorted([X1,X2|T]):-
	X1=<X2,
	sorted([X2|T]).
