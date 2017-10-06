findlast([],0).
findlast([X],X).
findlast([H|T],X):-
	length([H|T],Y),
	(Y =:= 1
		->
		X=H
		;findlast(T,X)
	).

length([],0).
length([H|T],X):-
	length(T, Prev),
	X is Prev +1.
	
	
%AnotherWay
findlast([_,E|T],X):-
	findlast([E|T],X).
	

	