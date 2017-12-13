second_last([],0).
second_last([H],0).
second_last([H,_],H).
second_last([_,H2, H3|T], X):-
	second_last([H2,H3|T],X).
