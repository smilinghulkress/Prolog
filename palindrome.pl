%Palindrome
:-import reverse/2 from basics.

palindrome(L):-
	reverse(L,L).