%%% -*- Mode: Prolog; -*-

:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

%:- style_check(all).

builtin(true).
builtin(findall(_,_,_)).
builtin(length(_,_)).
builtin(member(_,_)).
builtin(timestep(_)).
builtin(A=B).
builtin(A\==B).
builtin(A\=B).
builtin(A is B).
builtin(A > B).
builtin(A < B).
builtin(A >= B).
builtin(A =< B).
builtin(between(_,_,_)).
builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(sum_list(_,_)).
builtin(nth1(_,_,_)).
builtin(varia(_,_,_)).
builtin(product(_,_)).
builtin(densityGaussian(_,_,_,_)).

:- set_magic(false).


nballs ~ uniform([1,2,3,4,5,6,7,8,9,10]) := true.

n2 ~ finite([0.9:1,0.05:5,0.05:6]) := true.

n4 ~ uniform([A]) := n2~=N,drawn(1)~=B,A is N*B.

ball(X) := nballs ~= N, between(1,N,X).


draw(1) := true.
draw(2) := true.
draw(3) := true.



drawn(Draw) ~ uniform(Balls) := draw(Draw), nballs ~= N, findall(X,between(1,N,X),Balls).

g ~ gaussian(0,0.01) := true.

test :-
	init,
	time(eval_query([],[],drawn(1) ~= 10,1000,P,_,_)),
	write('probability: '),writeln(P).

:- initialization(test).
/*
init.
eval_query([],[],drawn(1) ~= 1,1000,P,A,B).

eval_query([],[],(g ~= A,A<0.1),1000,P,A,B).

eval_query([g ~= A,A>0],[],(g ~= A,A<0.1),1000,P,A,B).


*/	

