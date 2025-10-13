/*swap the first two elements if they are not in order*/ 
swap([X, Y|T], [Y, X | T]):-
	Y =< X. 
/*swap elements in the tail*/ 
swap([H|T], [H|T1]):- 
	swap(T, T1). 


/* 	
 	bubbleSort(L, SL): sorts list L into sorted list SL using     
 	the Bubble Sort algorithm. It repeatedly swaps adjacent       
	elements that are out of order until the list is sorted.      */

bubbleSort(L,SL):-
	swap(L, L1), % at least one swap is needed
	!, 
	bubbleSort(L1, SL). % recursively sorting the partially sorted list
bubbleSort(L, L). % here, the list is already sorted

/* 	ordered(): true if L is sorted in non-decreasing order.     */
/* 	Helper function for insertionSort.                          */
ordered([]).
ordered([_X]).
ordered([H1, H2|T]):-
	H1 =< H2, 
	ordered([H2|T]).

/* 	
	insert(E, SL, SLE): inserts element E into the sorted list SL 
 	producing the new sorted list SLE. Used in insertionSort.     */

/*	First clause: if E <= H, insert E before H in the list. */
insert(X, [],[X]). 
insert(E, [H|T], [E,H|T]):- 
	ordered(T),
	E=<H, 	% condition to check if E is less than or equal to H
                        !. 
/* 	Second clause: if E > H, keep H in place and insert E later. */
insert(E, [H|T], [H|T1]):- 
	ordered(T),
    insert(E, T, T1). % recursive call to insert E into the tail T to get T1

/*
	 insertionSort(L, SL): sorts list L into SL using              
 	 the Insertion Sort algorithm. It recursively sorts the tail   
	 and inserts the head into the correct position.               */
insertionSort([], []). 
insertionSort([H|T], SORTED) :- 
	insertionSort(T, T1), 
    insert(H, T1, SORTED).  % insert H into the sorted tail T1 to get SORTED


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
split_in_half([], _, _):-!, fail.	% Given an empty list, you cant split it in half
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
   

/* 	
	split(X, LIST, SMALL, BIG): divides LIST into two lists:      
  	SMALL contains elements <= X, BIG contains elements > X.      */
split(_, [],[],[]). 
split(X, [H|T], [H|SMALL], BIG):- 
	H =< X, 
	split(X, T, SMALL, BIG).  % if H is less than or equal to X, put H in SMALL

split(X, [H|T], SMALL, [H|BIG]):-
	X =< H,
	split(X, T, SMALL, BIG).  % if X is less than or equal to H, put H in BIG
	
/* 
	quickSort(L, SL): sorts list L into sorted list SL. It selects
	a pivot (head), splits remaining elements, and recursively
	sorts the sublists.       */
quickSort([], []).
quickSort([H|T], LS):-
	split(H, T, SMALL, BIG), % split T into SMALL and BIG based on pivot H
	quickSort(SMALL, S), % sort elements less than the pivot
	quickSort(BIG, B), 	% sort elements greater than the pivot
	append(S, [H|B], LS). % combining the lists to get the final sorted list


/* Chooses between two algorithms based on the size of LIST
   If the length of LIST is less than or equal to THRESHOLD,
   it uses SMALLALG to sort the list. If the length of LIST
   is larger than THRESHOLD, then behave like BIGALG
*/
hybridSort(LIST, SMALLALG, BIGALG, THRESHOLD, SLIST):-
	length(LIST, N), N=< THRESHOLD,
	call(SMALLALG, LIST, SLIST).		% When list size is smaller than or equal to threshold, use SMALLALG
										% Had to be switched from "SMALLALG(LIST, SLIST)." to "call(SMALLALG, LIST, SLIST)."

hybridSort(LIST, SMALLALG, BIGALG, THRESHOLD, SLIST):-
	length(LIST, N), N > THRESHOLD,
	(
		BIGALG = mergeSort -> 
		split_in_half(LIST, L1, L2),	% Behaves like mergeSort; splits then merges
		hybridSort(L1, SMALLALG, BIGALG, THRESHOLD, S1),
		hybridSort(L2, SMALLALG, BIGALG, THRESHOLD, S2),
		merge(S1, S2, SLIST);
	 	BIGALG = quickSort ->
		LIST = [P|T],					% Behaves like quickSort; picks pivot then splits	
		split(P, T, SMALL, BIG),
		hybridSort(SMALL, SMALLALG, BIGALG, THRESHOLD, S1),
		hybridSort(BIG, SMALLALG, BIGALG, THRESHOLD, S2),
		append(S1, [P|S2], SLIST)
	).