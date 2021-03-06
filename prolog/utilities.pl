list_to_tuple([],_).
list_to_tuple([X],X).
list_to_tuple([A,B],(A,B)).
list_to_tuple([A,B|T],(A,B,Rest_Tuple)) :-
    list_to_tuple(T,Rest_Tuple).
   
  
tuple_to_list((A,B,Rest_Tuple),[A,B|T]) :-
	tuple_to_list(Rest_tuple,T).
tuple_to_list((A,B),[A,B]).  	 
tuple_to_list((A),[A]).  

get_actionname(T,WA) :-
	atomic_concat([action,T],WA).

get_questionname(T,Q) :-
	atomic_concat([question,T],Q).

get_perfname(T,P) :-
	atomic_concat([perf,T],P).

get_non_grounded([],[]).
get_non_grounded([H|T],L) :-
	ground(H),
	writeln(ground: H),
	get_non_grounded(T,L).
get_non_grounded([H|T],[H2|T2]) :-
	writeln(nonground: H),
	H2 = H, 
	get_non_grounded(T,T2).

get_grounded([],L).
get_grounded([H|T],[H2|T2]) :-
	ground(H),
	writeln(ground: H),
	H2 = H,
	get_grounded(T,T2).
get_grounded([H|T],L) :-
	writeln(nonground: H),
	get_grounded(T,L).

get_values([],[]).
get_values([(Prob:Value)|R],[Value|R2]) :-
	get_values(R,R2).
	
get_probs([],[]).
get_probs([(Prob:(Value))|R],[Prob|R2]) :-
	get_probs(R,R2).


changelist([],[]).
changelist([(H1:H2)|R1],[(H1-H2)|R2]) :-
	changelist(R1,R2).

changelist2([],[]).
changelist2([(H1,H2)|R1],[(H1-H2)|R2]) :-
	changelist2(R1,R2).

changelist3([],[]).
changelist3([(H1-H2)|R1],[(H1:H2)|R2]) :-
	changelist3(R1,R2).
	
get_index(El,List,Index) :-
	length(List,Length),
	get_index2(El,List,Length,Index).

getPreList(_,0,[]).
getPreList([H|R],N,[H|R2]) :-
	N2 is N-1,
	getPreList(R,N2,R2).

distance([X1,Y1,Z1],(X2,Y2,Z2),Dist) :-
	distance((X1,Y1,Z1),(X2,Y2,Z2),Dist).
distance((X1,Y1,Z1),(X2,Y2,Z2),Dist) :-
	Dist is sqrt(exp((X1-X2),2)+exp((Y1-Y2),2)+exp((Z1-Z2),2)).

get_index2(El,List,0,0).
get_index2(El,List,I1,Index) :-
	nth(I1,List,El2),
	El == El2,
	Index is I1.
	
get_index2(El,List,I1,Index) :-
	I2 is I1-1,
	get_index2(El,List,I2,Index).

deleteElement(_,[],[]).
deleteElement(El,[H|R],R2) :-
	El = H,
	deleteElement(El,R,R2).
deleteElement(El,[H|R],[H|R2]) :-
	El \= H,
	deleteElement(El,R,R2).	
	

member2(X,[X|R]).
member2(X,[Y|R]) :- member2(X,R).

member3(X, [(_:X)|_]).
member3(X, [(_:_)|R]) :- member3(X,R).


