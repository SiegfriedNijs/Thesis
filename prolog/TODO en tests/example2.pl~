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

:- set_magic(backward).
:- set_debug(false).

nballs ~ uniform([1,2,3,4,5,6,7,8,9,10]) := true.

n2 ~ finite([0.9:1,0.05:2,0.05:3]) := true.

n4 ~ uniform([A]) := n2~=N,drawn(1)~=B,A is N*B.

ball(X) := nballs ~= N, between(1,N,X).


draw(1) := true.
draw(2) := true.
draw(3) := true.



drawn(Draw) ~ uniform(Balls) := draw(Draw), nballs ~= N, findall(X,between(1,N,X),Balls).

g ~ gaussian(0,0.01) := true.

pos(X,T) ~ gaussian([0,0],[25,0,0,25]) := true.

connected(A,B) ~ finite([0.8:false,0.2:true]) := true.

string(A,B) ~ contUniform(0,10) := 
	connected(A,B) ~= true.

distance(A,B,T) ~ uniform([Dist]) := 
	pos(A,T) ~= (PAx,PAy), 
	pos(B,T) ~= (PBx,PBy), 
	Dist is sqrt((PAx-PBx)^2+(PAy-PBy)^2).

%constraint(A,B) ~ finite([1:false]) := distance(A,B) ~= D, string(A,B) ~= S, D>L.
constraint(A,B,T) ~ finite([1:true]) := 
	distance(A,B,T) ~= D, 
	string(A,B) ~= S, 
	D<S.

constraint(A,B,T) ~ finite([1:true]) := 
	connected(A,B) ~= false.
	 
%test3(N,Error) :-
%	distributionalclause:eval_query_backward_exp([pos(1) ~=(0,0),pos(2) ~=(1,1)],[],string(1,2) ~= X,N,P1,_,_).

test(N,Error) :-
	%init,
	distributionalclause:eval_query_backward_exp([],[],drawn(1) ~= 1,N,P1,_,_),
	writeln(P1),
	E1 is abs(P1-( 0.1*(1+1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 1: '),writeln(E1),
	
	distributionalclause:eval_query_backward_exp([],[],drawn(1) ~= 2,N,P2,_,_),
	E2 is abs(P2-( 0.1*(1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 2: '),writeln(E2),
	
	distributionalclause:eval_query_backward_exp([],[],drawn(1) ~= 3,N,P3,_,_),
	E3 is abs(P3-( 0.1*(1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 3: '),writeln(E3),
	Error is E1+E2+E3.

test2(N,Error) :-
	%init,
	distributionalclause:eval_query_backward([],[],drawn(1) ~= 1,N,P1,_,_),
	E1 is abs(P1-( 0.1*(1+1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 1: '),writeln(E1),
	
	distributionalclause:eval_query_backward([],[],drawn(1) ~= 2,N,P2,_,_),
	E2 is abs(P2-( 0.1*(1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 2: '),writeln(E2),
	
	distributionalclause:eval_query_backward([],[],drawn(1) ~= 3,N,P3,_,_),
	E3 is abs(P3-( 0.1*(1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 3: '),writeln(E3),
	Error is E1+E2+E3.

test3(N,Error) :-
	%init,
	distributionalclause:eval_query_backward_exact([],[],drawn(1) ~= 1,N,P1,_,_),
	E1 is abs(P1-( 0.1*(1+1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 1: '),writeln(E1),
	
	distributionalclause:eval_query_backward_exact([],[],drawn(1) ~= 2,N,P2,_,_),
	E2 is abs(P2-( 0.1*(1/2+1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 2: '),writeln(E2),
	
	distributionalclause:eval_query_backward_exact([],[],drawn(1) ~= 3,N,P3,_,_),
	E3 is abs(P3-( 0.1*(1/3+1/4+1/5+1/6+1/7+1/8+1/9+1/10) )),
	%write('error drawn(1) ~= 3: '),writeln(E3),
	Error is E1+E2+E3.

test3_1(P1) :-
	distributionalclause:eval_query_backward_exact([],[],(string(1,2) ~= A,A>5),10,P1,_,_).

% distributionalclause:eval_query_backward_exact([n4~=2],[],drawn(1) ~= 1,1,P1,_,_).	
:- init.

repeat(Samples,Times,Error) :-
	findall(Error,(between(1,Times,I),test(Samples,Error)),L),
	sum_list(L,Sum),
	Error is Sum/Times.
	
repeat2(Samples,Times,Error) :-
	findall(Error,(between(1,Times,I),test2(Samples,Error)),L),
	sum_list(L,Sum),
	Error is Sum/Times.
	
plotdata_exp(N) :-
	open('data.txt','write',S),
	distributionalclause:eval_query_backward_exp_distrib([pos(1,0) ~=(0,0),pos(2,0) ~=(0,1),pos(3,0) ~=(2,0),constraint(1,2,0) ~= true,constraint(1,3,0) ~= true,constraint(3,2,0) ~= true,pos(1,1) ~=(0,2),pos(2,1) ~=(0,0),pos(3,1) ~=(0,0),constraint(1,2,1) ~= true,constraint(1,3,1) ~= true,constraint(3,2,1) ~= true],[],(X,Y),(string(1,2) ~= X,string(2,3) ~= Y),N,D),
	length(D,LN),writeln(LN),
	(
		member(W:(Vx,Vy),D),
		W>0,
		write(S,Vx),write(S,' '),write(S,Vy),nl(S),
		fail;
		true
	),
	close(S).
	
plotdata(N) :-
	open('data.txt','write',S),
	distributionalclause:eval_query_backward_distrib([pos(1,0) ~=(0,0),pos(2,0) ~=(0,1),pos(3,0) ~=(2,0),constraint(1,2,0) ~= true,constraint(1,3,0) ~= true,constraint(3,2,0) ~= true,pos(1,1) ~=(0,2),pos(2,1) ~=(0,0),pos(3,1) ~=(0,0),constraint(1,2,1) ~= true,constraint(1,3,1) ~= true,constraint(3,2,1) ~= true],[],(X,Y),(string(1,2) ~= X,string(2,3) ~= Y),N,D),
	length(D,LN),writeln(LN),
	(
		member(W:(Vx,Vy),D),
		W>0,
		write(S,Vx),write(S,' '),write(S,Vy),nl(S),
		fail;
		true
	),
	close(S).
/*
clean_sample(sampled),
abolish_all_tables,
distributionalclause:proof_query_backward(sampled,sampled,drawn(1) ~= _).
recorded(sampled,A,_).
clean_sample(sampled),
abolish_all_tables,
distributionalclause:proof_query_backward_exp(sampled,[],A,drawn(1) ~= 1).

distributionalclause:eval_query_backward_exp([],[],drawn(1) ~= 1,10000,P,S,Sum).
distributionalclause:eval_query_backward([],[],drawn(1) ~= 1,10000,P,S,Sum).


distributionalclause:eval_query_backward_exp([n4 ~= 2],[],drawn(1) ~= 1,10000,P,S,Sum).
distributionalclause:eval_query_backward([n4 ~= 2],[],drawn(1) ~= 1,10000,P,S,Sum).

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

