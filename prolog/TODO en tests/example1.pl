%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

%:- style_check(all).

:- set_magic(backward).
:- set_debug(false).

nballs ~ uniform([1,2,3,4,5,6,7,8,9,10]) := true.

n2 ~ finite([0.9:1,0.05:5,0.05:6]) := true.

n4 ~ uniform([2,N]) := n3~=N.

ball(X) := nballs ~= N, between(1,N,X).


%draw(1) := true.
%draw(2) := true.
%draw(3) := true.



drawn(Draw) ~ uniform(Balls) := nballs ~= N, findall(X,between(1,N,X),Balls).

g ~ gaussian(0,0.01) := true.

test(N) :-
	init,
	eval_query([],[],drawn(1) ~= 1,N,P1,_,_),
	E1 is abs(P1-( 0.1*(1+1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	write('error drawn(1) ~= 1: '),writeln(E1),
	
	eval_query([],[],drawn(1) ~= 2,N,P2,_,_),
	E2 is abs(P2-( 0.1*(1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	write('error drawn(1) ~= 2: '),writeln(E2),
	
	eval_query([],[],drawn(1) ~= 3,N,P3,_,_),
	E3 is abs(P3-( 0.1*(1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	write('error drawn(1) ~= 3: '),writeln(E3).

:- initialization(time(test(10000))).
/*
init.
eval_query([n3~=1],[],[],n4 ~= 1,100,P1,_,_).

init.
eval_query([],[],drawn(1) ~= 1,1000,P,A,B).

eval_query([],[],(g ~= A,A<0.1),1000,P,A,B).

eval_query([g ~= A,A>0],[],(g ~= A,A<0.1),1000,P,A,B).

time(eval_query([],[],drawn(1) ~= 1,1000,P,A,B)).
findall(A,(recorded(sampled,A,_),write(A),nl),_).
	

distributionalclause:eval_query([],[],n3 ~=4,100,P,_,_).
distributionalclause:eval_query_exact(nballs,[],[],n3 ~=4,10,P).

time((
bb_put(errore,0.0),
bb_put(media,0.0),
N=100,
(
	between(1,N,I),
	distributionalclause:eval_query([],[],n4 ~=15,600,P,_,_),
	bb_get(errore,E),
	NE is E+abs(P-0.00715),
	bb_put(errore,NE),
	bb_get(media,M),
	NM is M+P,
	bb_put(media,NM),
	fail;
	true
),
bb_delete(errore,DEF),
bb_delete(media,Med),
DM is Med/N,
write(DEF),nl,
write(DM),nl)).

time((
bb_put(errore,0.0),
bb_put(media,0.0),
N=100,
(
	between(1,N,I),
	distributionalclause:eval_query_exact(n2,[],[],n4 ~=15,180,P),
	bb_get(errore,E),
	NE is E+abs(P-0.00715),
	bb_put(errore,NE),
	bb_get(media,M),
	NM is M+P,
	bb_put(media,NM),
	fail;
	true
),
bb_delete(errore,DEF),
bb_delete(media,Med),
DM is Med/N,
write(DEF),nl,
write(DM),nl)).
*/	

