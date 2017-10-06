:- import length/2 from basics.

%findkth([],_,0).
findkth([H|_],1,H).
findkth(L,K,X):-
	length(L,Y),
	Y < K,
	X = 0.

findkth([H|T],K,X):-
	K>1,
	X2 is K -1,
	findkth(T,X2,X).
	