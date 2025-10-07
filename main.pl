/*swap the first two elements if they are not in order*/ 
swap([X, Y|T], [Y, X | T]):-
	Y =< X. 
/*swap elements in the tail*/ 
swap([H|T], [H|T1]):- 
	swap(T, T1). 


/* Comment describing bubbleSort */
bubbleSort(L,SL):-
	swap(L, L1), % at least one swap is needed
	!, 
	bubbleSort(L1, FILLINHERE). 
bubbleSort(L, L). % here, the list is already sorted

/* Comment describing ordered */
ordered([]).
ordered([_X]).
ordered([H1, H2|T]):-
	H1 =< H2, 
	ordered([H2|T]).

/*Comment describing insert(E, SL, SLE) …*/
/*Comment describing the 1st clause of insert …*/
insert(X, [],[X]). 
insert(E, [H|T], [E,H|T]):- 
	ordered(T),
FILLINHERE(E, H), 
                        !. 
/*Comment describing the 2nd clause of insert …*/
insert(E, [H|T], [H|T1]):- 
	ordered(T),
    insert(E, T, FILLINHERE). 

/* Comment describing insertionSort */
insertionSort([], []). 
insertionSort([H|T], SORTED) :- 
	insertionSort(T, T1), 
    insert(H, T1, FILLINHERE). 


/* mergeSort(L, SL): Given a list L, SL is a list of L's elements sorted.
	mergeSort works recursively. It splits the list L in half and calls
	mergeSort on each half. Then, it takes the two sorted lists and 
	passes it into function merge, which goes through the lists, popping
	off the smaller element of each, in order. */
mergeSort([], []).		% The empty list is sorted 
mergeSort([X], [X]):-!.	% A list with one element X is sorted.
mergeSort(L, SL):- 		% Unsorted list L, when sorted, becomes sorted list SL
	split_in_half(L, L1, L2), 
	mergeSort(L1, S1),	% recursively merge sort first half L1 into S1
	mergeSort(L2, S2),			% recursively merge sort second half L2 into S2
	merge(S1, S2, SL). 	% Merge the two halves (S1 and S2) together into SL.

/* split_in_half takes a list L and splits it into 
	L1, the list of the first half, and L2, the list for the second half. */
intDiv(N,N1, R):- R is div(N,N1).	% Divides N by N1 into result R, rounding down if there is a half remainder.
split_in_half([], _, _):-!, fail.	% Given an empty list, you can't split it in half
split_in_half([X],[],[X]). 	% A list of 1 element is split into an empty list L1 and
							%	list L2 containing the 1 element.
split_in_half(L, L1, L2):- 	% Given a list, L1 is the first half and L2 is the second half.
	length(L,N),			% N is the length of the list L
	intDiv(N,2,N1),			% N divided by 2 is N1
	length(L1, N1),			% Length of L1 is N1
	append(L1, L2, L). 		% L1 and L2 appended create L

/* merge(S1, S2, S): lists S1 and S2 can be merged to 
	get the full sorted list S. S1 and S2 are each sorted. 
	The first elements of S1 and S2 each are compared and
	added to S in order from smallest to largest. When added
	to S, the element is popped off the front of the list, 
	and merge is called again on the remaining elements in
	S1 and S2 to get the rest of S. */
merge([], L, L). % empty list and L merge to create L
merge(L, [],L).  % L and empty list merge to create L
merge([H1|T1],[H2|T2],[H1|T]):-
	H1 < H2,				% When H1 is less than H2, H1 is the start of the resulting merged list
	merge(T1,[H2|T2],T). 	% Recursively merge tail of first half and full second half to get T, 
							% 	the sorted rest of the list
merge([H1|T1], [H2|T2], [H2|T]):-
	H2 =< H1,				% When H2 is less than or equal to H1, H2 is at the start of the resulting merged list
	merge([H1|T1], T2, T).	% Recursively merge full first half of list with tail of second half to get T,
							%	the sorted rest of the list
   

/* Comment describing split for quickSort */
split(_, [],[],[]). 
split(X, [H|T], [H|SMALL], BIG):- 
	H =< X, 
	split(X, T, SMALL, FILLINHERE).    

split(X, [H|T], SMALL, [H|BIG]):-
	X =< H,
	split(X, T, FILLINHERE, BIG). 
	
/* Comment describing quickSort */
quickSort([], []).
quickSort([H|T], LS):-
	split(H, T, SMALL, FILLINHERE), 
	quickSort(SMALL, S), 
	quickSort(BIG, B), 
	append(S, [H|B], FILLINHERE). 


/* Comment describing hybridSort */
hybridSort(LIST, SMALLALG, BIGALG, THRESHOLD, SLIST):-
	length(LIST, N), N=< THRESHOLD,
	SMALLALG(LIST, FILLINHERE).

hybridSort(LIST, SMALLALG, BIGALG, THRESHOLD, SLIST):-
	length(LIST, N), N > THRESHOLD,
	FILLINHERE.  % Comment: fill in the behavior of BIGALG. 
