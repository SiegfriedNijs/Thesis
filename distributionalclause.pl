%%% -*- Mode: Prolog; -*-

:- module(distributionalclause,[log_likelihood_weighting/3,prod_scalar_multidim/3,eval_query_backward_exp/7,eval_query_backward_eval/7,matrixproduct/4,logIndepOptimalProposals/3,indepOptimalProposals/3,optimalproposal/7,set_debug/1,test_to_list/2,proof_query_backward_exp_eval/5,normalize/2,sum_list_multidim/3,divide_multidim/3,query_proof_defineRaoBackward/2,kalmanrao/14,kalmanrao_simplified/9,findmax/2,cleanDistribution/3,product_list/2,query_proof_setRaoBackward/5,sum_distrib/4,multiplyby/3,query_proof_rao/3,proof_query_backward_eval/4,proof_query_backward/3,proof_query_backward/2,timesyntax/2,likelihood_weighting/3,init/0,remove_builtin/2,prova/0,init_query_list/2,get_max_priority/1,derived/1,findManage/2,magic/0,init_query/2,eval_query_step/9,eval_distribution/7,magic_distributionalclause/4,magic_hardclause/0,magic_hardclause/3,magic_set_hard/3,magic_distributionalclause/0,magic_distributionalclause/3,evidence_proof_exists_maybe/1,sample/2,print_all/0,proof_exists_maybe/2,remove_inconsistent_value/5,check_value/2,clean_sample/1,sum_prob/2,divideby/3,normalize/2,assert_evidence/2,sample_lookahead/4, check_evidence/3,cumul/4,genesamplestep/4,generate_sample_pr/3,generate_sample/2,sample/2,query_proof/2,set_magic/1,get_magic/1,use_magic/1,montecarlo/3,montecarlo/4,eval_query/8,eval_query/7,findall_forward/3]).

:- use_module('random/sampling.pl').
:- use_module(library(lists)).

:- dynamic use_magic/1.
:- dynamic user:timestep/1.
:- dynamic user:deltaT/1.
:- dynamic user:evidence/2.
:- discontiguous user:(~)/2.
:- discontiguous user:(:=)/2.
:- dynamic user:(~)/2.
:- dynamic user:(~=)/2.
:- dynamic user:(:=)/2.
:- dynamic user:(pr)/2.
:- dynamic user:distributionalclause/4.
:- dynamic user:hardclause/3.
:- multifile user:builtin/1.
:- dynamic user:builtin/1.

%:- style_check(all).

:- op(690,xfx,user:'~').
:- op(681,xfx,user:'~=').
:- op(1100,xfx,user:':=').
:- op(1101,xfx,user:'pr').


%:- yap_flag(tabling_mode,local).

% tabling unstable!
:- table	tabling_proof_query_backward/2,  tabling_proof_query_backward/3,
		tabling_proof_query_backward2/2, tabling_proof_query_backward2/3.
		%logIndepOptimalProposals/3.
		%tabling_proof_query_backward_exp2/4, tabling_proof_query_backward_exp2/5.


% Buildin predicates: DC uses prolog instead of seeking them in the samples
user:builtin(true) :- !.
user:builtin(findall(_,_,_)) :- !.
user:builtin(length(_,_)) :- !.
user:builtin(member(_,_)) :- !.
user:builtin(timestep(_)) :- !.
user:builtin(A=B) :- !.
user:builtin(A\==B) :- !.
user:builtin(A\=B) :- !.
user:builtin(A is B) :- !.
user:builtin(A > B) :- !.
user:builtin(A < B) :- !.
user:builtin(A >= B) :- !.
user:builtin(A =< B) :- !.
user:builtin(integer(_)) :- !.
user:builtin(between(_,_,_)) :- !.
user:builtin(min_list(_,_)) :- !.
user:builtin(max_list(_,_)) :- !.
user:builtin(sum_list(_,_)) :- !.
user:builtin(min(_,_)) :- !.
user:builtin(nth1(_,_,_)) :- !.
user:builtin(nth0(_,_,_)) :- !.
user:builtin(sign(_)) :- !.
user:builtin(densityGaussian(_,_,_,_)) :- !.
user:builtin(\+A) :-
	user:builtin(A),!.
user:builtin(write(_)) :- !.
user:builtin(writeln(_)) :- !.
		
set_magic(V) :-
	retractall(use_magic(_)),
	assert(use_magic(V)).
	

ps :-
	findall(A,(recorded(sampled,A,_), write(A),nl),_).

/*
set_magic(false) :-
	retractall(use_magic(_)),
	assert(use_magic(false)).
*/
get_magic(V) :-
	use_magic(V).

set_debug(V) :-
	retractall(use_debug(_)),
	assert(use_debug(V)).
	
get_debug(V) :-
	use_debug(V).
% to substitute uniform((A,B):(C,D)) with uniform([(A,B),...,(C,D)])
user:term_expansion((H~uniform((A,B):(C,D)):=Body),(H~uniform(Distribution):=Body)) :-
	ground((A,B):(C,D)),
	findall((X,Y),(between(A,C,X),between(B,D,Y)),Distribution).

user:term_expansion((H~uniform(A:C) := Body),(H~uniform(Distribution) := Body)) :-
	ground(A:C),
	findall(X,between(A,C,X),Distribution).

user:term_expansion((H~val(V) := Body),(H~uniform([V]) := Body)).

user:term_expansion((H~val(V)),(H~uniform([V]) := true)).

user:term_expansion((H~uniform((A,B):(C,D)):=Body pr PR),(H~uniform(Distribution):=Body pr PR)) :-
	ground((A,B):(C,D)),
	findall((X,Y),(between(A,C,X),between(B,D,Y)),Distribution).

user:term_expansion((H~uniform(A:C) := Body pr PR),(H~uniform(Distribution) := Body pr PR)) :-
	ground(A:C),
	findall(X,between(A,C,X),Distribution).
	
/*
% if magic is off substitute H~D:=B with distributionalclause and H:=B with hardclause
user:term_expansion(H~D,distributionalclause(H,D,true,0)) :- get_magic(false).
user:term_expansion((H~D:=B),distributionalclause(H,D,B,0)) :- get_magic(false).
user:term_expansion((H:=B),hardclause(H,B,0)) :-
	get_magic(false),
	H\=_~_.

user:term_expansion((H~D pr X),distributionalclause(H,D,true,X)) :- get_magic(false).
user:term_expansion((H~D:=B pr X),distributionalclause(H,D,B,X)) :- get_magic(false).
user:term_expansion((H:=B pr X),hardclause(H,B,X)) :-
	get_magic(false),
	H\=_~_.
*/



%%% verify if a formula is proved


query_proof(Key,true) :-
	!.
query_proof(Key,(A,B)) :-
	!,
	query_proof(Key,A),
	query_proof(Key,B).

% negation, to check
query_proof(Key,\+A) :-
	(
		user:builtin(A)
		->
		(
			%A=findall_forward(X,Y,Z)
			%->
			%	\+findall(X,query_proof(Key,Y),Z);
			\+user:A%,
%			write('false '),write(A),nl
		)
		;
		(
			%trace,
			
			
			\+recorded(Key,A,_)
		)
		
	).

query_proof(Key,A) :-
	A\=(\+_),
	(
		user:builtin(A)
		->
		(
			A=findall_forward(X,Y,Z)
			->
				findall(X,query_proof(Key,Y),Z);
				user:A
		)
		;
		(
			
			recorded(Key,A,_)
		)
	).

query_proof_rao(Key,true,1.0) :-
	!.
query_proof_rao(Key,(A,B),W) :-
	!,
	query_proof_rao(Key,A,W1),
	query_proof_rao(Key,B,W2),
	W is W1*W2.


query_proof_rao(Key,current(A) ~= Val,W) :-
	user:rao(A),
	recorded(Key,current(A) ~= finite(Distribution),_),
	member(W:Val,Distribution).
	
query_proof_rao(Key,next(A) ~= Val,W) :-
	user:rao(A),
	recorded(Key,next(A) ~= finite(Distribution),_),
	member(W:Val,Distribution).
	%findall(W:Val,member(W:Val,Distribution),List).
	
query_proof_rao(Key,current(A) ~= Val,0.0) :-
	user:rao(A),
	recorded(Key,current(A) ~= finite(Distribution),_),
	\+member(W:Val,Distribution).
	
query_proof_rao(Key,next(A) ~= Val,0.0) :-
	user:rao(A),
	recorded(Key,next(A) ~= finite(Distribution),_),
	\+member(W:Val,Distribution).

query_proof_rao(Key,current(A) ~= Val,1.0) :-
	\+user:rao(A),
	(
		get_magic(backward) ->
			proof_query_backward(Key,current(A) ~= Val)
		;
			query_proof(Key,current(A) ~= Val)
	).
	
query_proof_rao(Key,next(A) ~= Val,1.0) :-
	\+user:rao(A),
	(
		get_magic(backward) ->
			proof_query_backward(Key,next(A) ~= Val)
		;
			query_proof(Key,next(A) ~= Val)
	).
	
query_proof_rao(Key,A,1.0) :-
	A\= _ ~= _,
	A\= (\+ _),
	(
		get_magic(backward) ->
			proof_query_backward(Key,A)
		;
			query_proof(Key,A)
	).

%TO TEST

query_proof_rao(Key,\+A,W) :-
	(
		query_proof_rao(Key,A,W1) ->
		(
			W is 1-W1
		)
		;
		(
			W=1
		)
	).



/*
query_proof_rao(Key,A ~= Val,W) :-
	(
		recorded(Key,A ~= finite(Distribution),_) ->
		(
			true
		)
		;
		(
			A=next(F),
			user:distributionalclause(bk(F),finite(Distribution),Body,_),
			ground(F),
			ground(Distribution),
			ground(Body),
			Body,
			recorda(Key,next(F) ~= finite(Distribution),_)
		)
	),
	!,
	recorded(Key,A ~= finite(Distribution),_),
	member(W:Val,Distribution).
	
	%findall(W:Val,member(W:Val,Distribution),List).

query_proof_rao(Key,A,1.0) :-
	A\= current(_) ~= finite(_),
	A\= next(_) ~= finite(_),
	query_proof(Key,A).% to check!
	
query_proof_rao(Key,\+A,1.0) :-
	A\= current(_) ~= finite(_),
	A\= next(_) ~= finite(_),
	query_proof(Key,\+A).
*/	


query_proof_setRaoBackward(Key,true,Var,Val,0) :-
	!.
query_proof_setRaoBackward(Key,(A,B),Var,Val,Ignore) :-
	!,
	query_proof_setRaoBackward(Key,A,Var,Val,Ignore1),
	(
		Ignore1==1 ->
			true
		;
			query_proof_setRaoBackward(Key,B,Var,Val,Ignore)
	).

query_proof_setRaoBackward(Key,A,Var,Val,Ignore) :-
	(
		A= next(Var) ~= V ->
		(
			V=Val,
			Ignore=0
		)
		;
		(
			(A= next(Var2) ~= V,user:rao(Var2),Var2\=Var) ->
			(
				Ignore=1
			)
			;
			(
				Ignore=0,
				proof_query_backward(Key,A)
			)
		)
	).


%used in add_rao_backward to add rao variables needed to evaluate the evidence 
query_proof_defineRaoBackward(Key,true) :-
	!.
query_proof_defineRaoBackward(Key,(A,B)) :-
	!,
	query_proof_defineRaoBackward(Key,A),
	query_proof_defineRaoBackward(Key,B).

query_proof_defineRaoBackward(Key,A) :-
	(
	
		(A= next(Var) ~= _,user:rao(Var)) ->
		(
			(\+recorded(Key,next(Var) ~= _,_),\+recorded(Key,current(Var) ~= _,_)) ->
			(
				user:distributionalclause(current(Var),Distribution,Body,_),
				proof_query_backward(Key,Body),
				ground(Var),
				ground(Distribution),
				recorda(Key,current(Var) ~= Distribution,_),
				write(current(Var) ~= Distribution),nl
			)
			;
			true
		)
		;
		(
			proof_query_backward(Key,A)
		)

	).


% verify the query A (2nd argument) with next(Var)=Val
% Ignore used for pruning resolution (body referred to another rao variable)

query_proof_setRao(Key,true,Var,Val,0) :-
	!.
query_proof_setRao(Key,(A,B),Var,Val,Ignore) :-
	!,
	query_proof_setRao(Key,A,Var,Val,Ignore1),
	(
		Ignore1==1 ->
			true
		;
			query_proof_setRao(Key,B,Var,Val,Ignore)
	).

query_proof_setRao(Key,A,Var,Val,Ignore) :-
	(
		A= next(Var) ~= V ->
		(
			V=Val,
			Ignore=0
		)
		;
		(
			(A= next(Var2) ~= V,user:rao(Var2),Var2\=Var) ->
			(
				Ignore=1
			)
			;
			(
				Ignore=0,
				query_proof(Key,A)
			)
		)
	).


proof_query_backward(Key,true) :-
	!.
	
proof_query_backward(Key,(A,B)) :-
	!,
	proof_query_backward(Key,A),
	proof_query_backward(Key,B).

	
% TO CHECK
proof_query_backward(Key,findall_forward(X,Y,Z)) :-
	findall(X,proof_query_backward(Key,Y),Z),
	!.



proof_query_backward(Key,\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward(Key,A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_query_backward(Key,\+A) :-
	\+proof_query_backward(Key,A),!.	

proof_query_backward(Key,A) :-
	ground(A),
%	A\= _~= distribution(_),
	recorded(Key,A,_),
	!.
	
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward(Key,H ~= S) :-
	ground(H~=S),
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
	erase(R),
	recorda(Key,H ~= Val,_),
	S=Val,
	!.
	
proof_query_backward(Key,A) :-
	recorded(Key,A,_),
	A\= _~= distribution(_).


% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward(Key,H ~= S) :-
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
	erase(R),
	(
	recorded(Key,H ~= V,_) -> % TO TEST
		(
			V=S,
			write('warning '),
			writeln(recorded(Key,H ~= distribution(D),R)),
			writeln( proof_query_backward(Key,H ~= S)),
			dcpf:printkeyp(Key),nl,
			erase(R)
		)
		;
		(
		recorda(Key,H ~= Val,_),
		S=Val
		)
	
	).
/*
proof_query_backward(Key,H ~= S) :-
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
	erase(R),
	recorda(Key,H ~= Val,_),
	S=Val.
*/	

%%% Tabling %%%
proof_query_backward(Key,Head ~= Val) :-
	tabling_proof_query_backward(Key,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Key,Head ~= Var,_),
	Var=Val.

proof_query_backward(Key,Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward(Key,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).

%proof_query_backward(Key,A) :-
%	recorded(Key,A,_).

tabling_proof_query_backward(Key,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
 	proof_query_backward(Key,Body).

	
tabling_proof_query_backward(Key,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward(Key,Body).

%%% New NOT TESTED %%%
/*
init_particle(3).
recorda(global,distributionalclause(current(maze(A,B)),finite([0.0:pit,0.6:free,0.4:wall]),((A,B)\=(0,0)),0),_).
spy distributionalclause:sample/2,
step_particle([action(left)],[observation(energy) ~= 0.918774012016998,observation(up) ~= wall,observation(right) ~= wall,observation(down) ~= wall,observation(left) ~= wall],3,1).


recorda(global,distributionalclause(current(wumpus),finite([0.0204081632653061:(-3,-3),0.0204081632653061:(-3,-2),0.0204081632653061:(-3,-1),0.0204081632653061:(-3,0),0.0204081632653061:(-3,1),0.0204081632653061:(-3,2),0.0204081632653061:(-3,3),0.0204081632653061:(-2,-3),0.0204081632653061:(-2,-2),0.0204081632653061:(-2,-1),0.0204081632653061:(-2,0),0.0204081632653061:(-2,1),0.0204081632653061:(-2,2),0.0204081632653061:(-2,3),0.0204081632653061:(-1,-3),0.0204081632653061:(-1,-2),0.0204081632653061:(-1,-1),0.0204081632653061:(-1,0),0.0204081632653061:(-1,1),0.0204081632653061:(-1,2),0.0204081632653061:(-1,3),0.0204081632653061:(0,-3),0.0204081632653061:(0,-2),0.0204081632653061:(0,-1),0.0204081632653061:(0,0),0.0204081632653061:(0,1),0.0204081632653061:(0,2),0.0204081632653061:(0,3),0.0204081632653061:(1,-3),0.0204081632653061:(1,-2),0.0204081632653061:(1,-1),0.0204081632653061:(1,0),0.0204081632653061:(1,1),0.0204081632653061:(1,2),0.0204081632653061:(1,3),0.0204081632653061:(2,-3),0.0204081632653061:(2,-2),0.0204081632653061:(2,-1),0.0204081632653061:(2,0),0.0204081632653061:(2,1),0.0204081632653061:(2,2),0.0204081632653061:(2,3),0.0204081632653061:(3,-3),0.0204081632653061:(3,-2),0.0204081632653061:(3,-1),0.0204081632653061:(3,0),0.0204081632653061:(3,1),0.0204081632653061:(3,2),0.0204081632653061:(3,3)]),true,0),_).

(distributionalclause:proof_query_backward_lifted(global,next(A)),fail;true).
(recorded(global,A,_),write(A),nl,fail;true).
distributionalclause:current2next(global).
(recorded(global,A,_),write(A),nl,fail;true).
*/

%tabling_proof_query_backward(Key,Head,Distribution) :-
%	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
%	proof_query_backward(Key,Body).

proof_query_backward(Key,Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward(Key,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Key,Head ~= Var,_),
	Var=Val.


computeDistribution(finite(List),[(Val,finite(List2))],finite(List3)) :-
	findall(P:V,(member(P2:V2,List2),V2=Val,member(P1:V,List),P is P1*P2),NewDist),
	compactDistribution(finite(NewDist),finite(List3)).

compactDistribution(finite(List),finite(NewList)) :-
	length(List,Length),
	bb_put(templist,[]),
	(
		between(1,Length,I),
		nth1(I,List,P:Elem),
		bb_get(templist,TempList),
		\+member(_:Elem,TempList),
		bb_put(tempelem,P:Elem),
		NextI is I+1,
		(
			between(NextI,Length,I2),
			nth1(I2,List,P2:Elem),
			bb_get(tempelem,PP:Elem),
			PNew is PP+P2,
			bb_put(tempelem,PNew:Elem),
			fail;
			true
		),
		bb_get(tempelem,PP:Elem),
		PP>0,
		bb_get(templist,LL),
		bb_put(templist,[PP:Elem|LL]),
		fail;
		true
	),
	bb_get(templist,NewList).
	

proof_query_backward_lifted(Key,next(Head)) :-
	user:distributionalclause(next(Head),Distribution,Body,_),
 	proof_query_backward_clause(Key,Body,Body2,ListDistributions),
	%\+ground(Body2),
	computeDistribution(Distribution,ListDistributions,NewDistr),
	test_to_list(NewBody,Body2),
	\+recorded(Key,distributionalclause(next(Head),_,NewBody,_),_),
	recorda(Key,distributionalclause(next(Head),NewDistr,NewBody,0),_).

current2next(Key) :-
	(
		recorded(Key,distributionalclause(current(Head),_,_,_),Ref),
		erase(Ref),
		fail;
		true
	),
%	print_distributionalclause_global,
	(
		recorded(Key,distributionalclause(next(Head),D,B,P),Ref),
		erase(Ref),
		\+(notsatisfiable(B)),
		recorda(Key,distributionalclause(current(Head),D,B,P),_),
		fail;
		true
	).

% not working
compact_body(Body,Output) :-
	test_to_list(Body,BodyList),
	BodyList=[A],
	Output=Body.

compact_body((HBody,TBody),Output) :-
	test_to_list((HBody,TBody),BodyList),
	BodyList=[H|T],
	duplicate_term(H,H2),
	duplicate_term(T,T2),
	member(H2,T),
	H==H2,
	T==T2,
	compact_body(TBody,Output).

compact_body((HBody,TBody),(HBody,Output)) :-
	test_to_list((HBody,TBody),BodyList),
	BodyList=[H|T],
	\+member(H,T),
	compact_body(TBody,Output).
	
notsatisfiable((H,T)) :-
	notsatisfiable(T).
	
notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A>B),
	member(A=<B,T).

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A<B),
	member(A>=B,T).

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A>=B),
	member(A<B,T).

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A=<B),
	member(A>B,T).

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A>B),
	ground(B),
	(member(A=<C,T);member(A<C,T)),
	ground(C),
	B>=C.

notsatisfiable(Body) :-
	test_to_list(Body,BodyList),
	BodyList=[H|T],
	H= (A<B),
	ground(B),
	(member(A>=C,T);member(A>C,T)),
	ground(C),
	B=<C.
	
proof_query_backward_clause(Key,true,[],[]) :-
	!.
	
proof_query_backward_clause(Key,(A,B),Body,ListDistr) :-
	!,
	proof_query_backward_clause(Key,A,BodyA,ListA),
	proof_query_backward_clause(Key,B,BodyB,ListB),
	append(ListA,ListB,ListDistr),
	append(BodyA,BodyB,Body).


proof_query_backward_clause(Key,A,[A],[]) :-
	user:satisfiable(A),
	\+ground(A),!.


proof_query_backward_clause(Key,Head ~= Val,B,[(Val,Distribution)|D]) :-
	\+ground(Val),
	recorded(Key,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_clause(Key,Body,B,D).

%proof_query_backward_clause(Key,A,[],[]) :-
%	proof_query_backward(Key,A).
	
proof_query_backward_clause(Key,\+A,[],[]) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_clause(Key,A,[],[]) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.	

% TO CHECK
proof_query_backward_clause(Key,\+A,B,D) :-
	\+proof_query_backward_clause(Key,A,B,D),!.	


%%% End NEW %%%


	
% with a temporary index to store sampled variables
% don't use Key=Temp!

proof_query_backward(Key,Key,_) :-
	!,
	writeln('error proof_query_backward: Key=Temp'),
	!.

proof_query_backward(Key,Temp,true) :-
	!.
	
proof_query_backward(Key,Temp,(A,B)) :-
	!,
	proof_query_backward(Key,Temp,A),
	proof_query_backward(Key,Temp,B).


% Really slow!
proof_query_backward(Key,Temp,findall_forward(X,Y,Z)) :-
	findall(X,proof_query_backward(Key,Temp,Y),Z),
	!.

proof_query_backward(Key,Temp,\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward(Key,Temp,A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_query_backward(Key,Temp,\+A) :-
	\+proof_query_backward(Key,Temp,A),!.

proof_query_backward(Key,Temp,A) :-
	ground(A),
%	A\= _~= distribution(_),
	recorded(Key,A,_),
	!.

proof_query_backward(Key,Temp,A) :-
	ground(A),
%	A\= _~= distribution(_),
	recorded(Temp,A,_),
	!.
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward(Key,Temp,H ~= S) :-
	ground(H~=S),
	recorded(Key,H ~= distribution(D),R),
	%\+recorded(Temp,H ~= _,_), % is always true
	sample(D,Val),
%	erase(R),
	recorda(Temp,H ~= Val,_),
	S=Val,
	!.
%%% Tabling %%%

proof_query_backward(Key,Temp,A) :-
	recorded(Key,A,_),
	A\= _~= distribution(_).
	
proof_query_backward(Key,Temp,A) :-
	recorded(Temp,A,_),
	A\= _~= distribution(_).

% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward(Key,Temp,H ~= S) :-
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	recorda(Temp,H ~= Val,_),
	S=Val.

proof_query_backward(Key,Temp,Head ~= Val) :-
	tabling_proof_query_backward2(Key,Temp,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward(Key,Temp,Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward(Key,Temp,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward(Key,Temp,Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward2(Key,Temp,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	\+recorded(Temp,Head,_),
	recorda(Temp,Head,_).
	
tabling_proof_query_backward2(Key,Temp,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
	proof_query_backward(Key,Temp,Body).
	
tabling_proof_query_backward2(Key,Temp,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward(Key,Temp,Body).
	
	
% New inference
	
% with a list to store sampled variables. during backtracking sampled random variables may be removed.
proof_query_backward_exp(Key,List,[],true) :-
	!.
	
proof_query_backward_exp(Key,List,NewList,(A,B)) :-
	!,
	proof_query_backward_exp(Key,List,NewList1,A),
	append([List,NewList1],List2),
	proof_query_backward_exp(Key,List2,NewList2,B),
	append([NewList1,NewList2],NewList).


% NOT COMPLETE!
proof_query_backward_exp(Key,Temp,[],findall_forward(X,Y,Z)) :-
	findall(X,proof_query_backward_exp(Key,Temp,Temp2,Y),Z),
	!.

proof_query_backward_exp(Key,List,[],\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_exp(Key,List,[],A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK problem with negation, if proof_query_backward_exp fails I need to consider the sampled random variables in NewList, that remains ungrounded
%proof_query_backward_exp(Key,List,NewList,\+A) :-
%	\+proof_query_backward_exp(Key,List,NewList,A),!.
% temporary fix!
proof_query_backward_exp(Key,List,[],\+A) :-
	\+proof_query_backward_exp(Key,List,NewList,A),!.

proof_query_backward_exp(Key,List,[],A) :-
	ground(A),
	recorded(Key,A,_),
	A\= _~= distribution(_),
	!.

proof_query_backward_exp(Key,List,[],A) :-
	ground(A),
	memberchk(A,List),
	!.


proof_query_backward_exp(Key,List,[H ~= Val],H ~= S) :-
	ground(H~=S),
	recorded(Key,H ~= distribution(D),R),
	sample(D,Val),
%	recorda(Key,H ~= Val,_),
	S=Val,
	!.

proof_query_backward_exp(Key,List,[H ~= Val],H~=Val) :-
%	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	\+member(H~=_,List),
	sample(D,Val).


	
%%% Tabling %%%

proof_query_backward_exp(Key,List,[],A) :-
	recorded(Key,A,_),
	A\= _~= distribution(_).
	
proof_query_backward_exp(Key,List,[],A) :-
	member(A,List).

proof_query_backward_exp(Key,List,[Head ~= Var|Newvars],Head ~= Val) :-
	tabling_proof_query_backward_exp2(Key,List,Newvars,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	append([List,Newvars],Temp),
	\+member(Head ~= _,Temp), %\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	%recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_exp(Key,List,[Head ~= Var|Newvars],Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_exp(Key,List,Newvars,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	append([List,Newvars],Temp),
	\+member(Head ~= _,Temp),%\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	%recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_exp(Key,List,[Head|Newvars],Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_exp2(Key,List,Newvars,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	append([List,Newvars],Temp),
	\+member(Head,Temp).%\+recorded(Temp,Head,_),
	%recorda(Temp,Head,_).
	
tabling_proof_query_backward_exp2(Key,List,Newvars,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
	proof_query_backward_exp(Key,List,Newvars,Body).
	
tabling_proof_query_backward_exp2(Key,List,Newvars,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_exp(Key,List,Newvars,Body).


proof_query_backward_exp_eval(Key,List,[],true,1.0) :-
	!.


proof_query_backward_exp_eval(Key,List,NewList,(A,B),W) :-
	!,
	proof_query_backward_exp_eval(Key,List,NewList1,A,W1),
	append([List,NewList1],List2),
	proof_query_backward_exp_eval(Key,List2,NewList2,B,W2),
	append([NewList1,NewList2],NewList),
	W is W1*W2.

% NOT IMPLEMENTED
%proof_query_backward_exp_eval(Key,Temp,findall_forward(X,Y,Z),1.0) :-
%	findall(X,proof_query_backward(Key,Temp,Y),Z),
%	!.

proof_query_backward_exp_eval(Key,List,[],\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_exp_eval(Key,List,Newvars,\+A,1.0) :-
	\+proof_query_backward_exp(Key,List,Newvars,A),!.

proof_query_backward_exp_eval(Key,List,Newvars,\+A,0.0) :-
	proof_query_backward_exp(Key,List,Newvars,A).

proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
	ground(A),
	A\=(\+_),
	A\= _~= distribution(_),
	recorded(Key,A,_),
	!.


proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
	ground(A),
	A\=(\+_),
	memberchk(A,List),%recorded(Temp,A,_),
	!.	
	
% TO TEST!!
% to support non-sampled variables H ~= distribution(D) in the particles
proof_query_backward_exp_eval(Key,List,[H ~= Val],H~=Val,W) :-
%	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	\+member(H~=_,List),
	likelihood_weighting(Val,D,W).


proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_),
	A\= _~= distribution(_).
	
proof_query_backward_exp_eval(Key,List,[],A,1.0) :-
	A\=(\+_),
	member(A,List).%recorded(Temp,A,_).

proof_query_backward_exp_eval(Key,List,[Head ~= Var|Newvars],Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward_exp(Key,List,Newvars,Body),
	 	append([List,Newvars],List2),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W)%,
				%recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				%recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_exp_eval(Key,List,[Head ~= Var|Newvars],Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward_exp(Key,List,Newvars,Body),
	 	append([List,Newvars],List2),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W)%,
				%recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				%recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_exp_eval(Key,List,[Head|Newvars],Head,1.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward_exp(Key,List,Newvars,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	append([List,Newvars],List2),
	\+member(Head,List2).%\+recorded(Temp,Head,_),
	%recorda(Temp,Head,_).


%proof_query_backward_exp_eval(Key,List,Newvars,Head,0.0) :-
%	Head\= _ ~= _,
%	user:hardclause(Head,Body,_),
%	\+proof_query_backward_exp(Key,List,Newvars,Body).	

%proof_query_backward_eval(Key,Temp,A,0.0) :-
%	\+proof_query_backward_eval(Key,Temp,A,_).

check_evidence_backward_exp(Key,List,Newvars,Wtot) :-
	findall(H,user:evidence(H,1),L),
	check_evidence_proof_exp(Key,List,Newvars,L,Wtot).

check_evidence_proof_exp(Key,List,[],[],1.0) :- !.

check_evidence_proof_exp(Key,List,Newvars3,[H|ListEvidence],W) :-
	proof_query_backward_exp_eval(Key,List,Newvars,H,W1),
	append([List,Newvars],List2),
	check_evidence_proof_exp(Key,List2,Newvars2,ListEvidence,W2),
	append([Newvars,Newvars2],Newvars3),
	W is W1*W2,
	!.
	
%%%% end new inference %%%

%%% exact %%%
% New inference
	
% with a list to store sampled variables
proof_query_backward_exact(Key,List,[],true) :-
	!.
	
proof_query_backward_exact(Key,List,NewList,(A,B)) :-
	!,
	proof_query_backward_exact(Key,List,NewList1,A),
	append([List,NewList1],List2),
	proof_query_backward_exact(Key,List2,NewList2,B),
	append([NewList1,NewList2],NewList).


% NOT COMPLETE!
proof_query_backward_exact(Key,Temp,[],findall_forward(X,Y,Z)) :-
	findall(X,proof_query_backward_exact(Key,Temp,Temp2,Y),Z),
	!.

proof_query_backward_exact(Key,List,[],\+A) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_exact(Key,List,[],A) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_query_backward_exact(Key,List,NewList,\+A) :-
	\+proof_query_backward_exact(Key,List,NewList,A),!.

proof_query_backward_exact(Key,List,[],A) :-
	ground(A),
	recorded(Key,A,_),
	!.

proof_query_backward_exact(Key,List,[],A) :-
	ground(A),
	memberchk(A,List),
	!.

%%% Tabling %%%

proof_query_backward_exact(Key,List,[],A) :-
	recorded(Key,A,_).
	
proof_query_backward_exact(Key,List,[],A) :-
	member(A,List).

proof_query_backward_exact(Key,List,[Head ~= Var|Newvars],Head ~= Val) :-
	tabling_proof_query_backward_exact2(Key,List,Newvars,Head,Distribution),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	append([List,Newvars],Temp),
	\+member(Head ~= _,Temp), %\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	%recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_exact(Key,List,[Head ~= Var|Newvars],Head ~= Val) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
 	proof_query_backward_exact(Key,List,Newvars,Body),
	ground(Head),
	ground(Distribution),
	\+recorded(Key,Head ~= _,_),
	append([List,Newvars],Temp),
	\+member(Head ~= _,Temp),%\+recorded(Temp,Head ~= _,_),
	sample(Distribution,Var),
	%recorda(Temp,Head ~= Var,_),
	Var=Val.

proof_query_backward_exact(Key,List,[Head|Newvars],Head) :-
	Head\=(_ ~= _),
	tabling_proof_query_backward_exact2(Key,List,Newvars,Head),	
	ground(Head),
	\+recorded(Key,Head,_),
	append([List,Newvars],Temp),
	\+member(Head,Temp).%\+recorded(Temp,Head,_),
	%recorda(Temp,Head,_).
	
tabling_proof_query_backward_exact2(Key,List,Newvars,Head,Distribution) :-
	user:distributionalclause(Head,Distribution,Body,_),
	proof_query_backward_exact(Key,List,Newvars,Body).
	
tabling_proof_query_backward_exact2(Key,List,Newvars,Head) :-
	user:hardclause(Head,Body,_),
	proof_query_backward_exact(Key,List,Newvars,Body).


proof_query_backward_exact_eval(Key,List,[],true,1.0) :-
	!.


proof_query_backward_exact_eval(Key,List,NewList,(A,B),W) :-
	!,
	proof_query_backward_exact_eval(Key,List,NewList1,A,W1),
	append([List,NewList1],List2),
	proof_query_backward_exact_eval(Key,List2,NewList2,B,W2),
	append([NewList1,NewList2],NewList),
	W is W1*W2.

% NOT IMPLEMENTED
%proof_query_backward_exact_eval(Key,Temp,findall_forward(X,Y,Z),1.0) :-
%	findall(X,proof_query_backward(Key,Temp,Y),Z),
%	!.

proof_query_backward_exact_eval(Key,List,[],\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_exact_eval(Key,List,Newvars,\+A,1.0) :-
	\+proof_query_backward_exact(Key,List,Newvars,A),!.

%proof_query_backward_exact_eval(Key,List,Newvars,\+A,0.0) :-
%	proof_query_backward_exact(Key,List,Newvars,A).

proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
	ground(A),
	A\=(\+_),
	recorded(Key,A,_),
	!.

proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
	ground(A),
	A\=(\+_),
	memberchk(A,List),%recorded(Temp,A,_),
	!.

proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_).
	
proof_query_backward_exact_eval(Key,List,[],A,1.0) :-
	A\=(\+_),
	member(A,List).%recorded(Temp,A,_).

proof_query_backward_exact_eval(Key,List,[Head ~= Var|Newvars],Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward_exact_eval(Key,List,Newvars,Body,Wold),
	 	append([List,Newvars],List2),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W1)%,
				%recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				exactsampling(Distribution,Var,W1)%, sample(Distribution,Var),
				%recorda(Temp,Head ~= Var,_),
				%W=1.0
			)
		)
	),
	W is Wold*W1.
/*
proof_query_backward_exact_eval(Key,List,[Head ~= Var|Newvars],Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward_exact(Key,List,Newvars,Body),
	 	append([List,Newvars],List2),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W)%,
				%recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+member(Head ~= _,List2),%\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				%recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).
*/
proof_query_backward_exact_eval(Key,List,[Head|Newvars],Head,W) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward_exact_eval(Key,List,Newvars,Body,W),
	ground(Head),
	\+recorded(Key,Head,_),
	append([List,Newvars],List2),
	\+member(Head,List2).%\+recorded(Temp,Head,_),
	%recorda(Temp,Head,_).

check_evidence_backward_exact(Key,List,Newvars,Wtot) :-
	findall(H,user:evidence(H,1),L),
	check_evidence_proof_exact(Key,List,Newvars,L,Wtot).

check_evidence_proof_exact(Key,List,[],[],1.0) :- !.

check_evidence_proof_exact(Key,List,Newvars3,[H|ListEvidence],W) :-
	proof_query_backward_exact_eval(Key,List,Newvars,H,W1),
	append([List,Newvars],List2),
	check_evidence_proof_exact(Key,List2,Newvars2,ListEvidence,W2),
	append([Newvars,Newvars2],Newvars3),
	W is W1*W2.
%%% end exact %%%

% don't use Key=Temp
proof_query_backward_eval(Key,Temp,true,1.0) :-
	!.

% error in proof_query_backward_eval	
proof_query_backward_eval(Key,Temp,(A,B),W) :-
	!,
	proof_query_backward_eval(Key,Temp,A,W1),
	proof_query_backward_eval(Key,Temp,B,W2),
	W is W1*W2.

% TO CHECK
proof_query_backward_eval(Key,Temp,findall_forward(X,Y,Z),1.0) :-
	findall(X,proof_query_backward(Key,Temp,Y),Z),
	!.

proof_query_backward_eval(Key,Temp,\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_eval(Key,Temp,A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_eval(Key,Temp,\+A,1.0) :-
	\+proof_query_backward(Key,Temp,A),!.


proof_query_backward_eval(Key,Temp,A,1.0) :-
	ground(A),
	A\=(\+_),
	A\= _~= distribution(_),
	recorded(Key,A,_),
	!.

proof_query_backward_eval(Key,Temp,A,1.0) :-
	ground(A),
	A\=(\+_),
	A\= _~= distribution(_),
	recorded(Temp,A,_),
	!.
	
% to support non-sampled variables H ~= distribution(D) in the particles	
proof_query_backward_eval(Key,Temp,H~=Val,W) :-
	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	likelihood_weighting(Val,D,W),
	recorda(Temp,H~=Val,_),
	!.
/*
proof_query_backward_eval(Key,Temp,H~=Val,W) :-
	ground(H~=Val),
	recorded(Temp,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	likelihood_weighting(Val,D,W),
	recorda(Temp,H~=Val,_),
	!.
*/	
%%% Tabling %%%

proof_query_backward_eval(Key,Temp,A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_),
	A\= _~= distribution(_).
	
proof_query_backward_eval(Key,Temp,A,1.0) :-
	A\=(\+_),
	recorded(Temp,A,_),
	A\= _~= distribution(_).

proof_query_backward_eval(Key,Temp,H ~= S,1.0) :-
	\+ground(S),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	recorda(Temp,H ~= Val,_),
	S=Val.
	
proof_query_backward_eval(Key,Temp,Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward(Key,Temp,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_eval(Key,Temp,Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward(Key,Temp,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Temp,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				\+recorded(Temp,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Temp,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_eval(Key,Temp,Head,1.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward(Key,Temp,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	\+recorded(Temp,Head,_),
	recorda(Temp,Head,_).

/*
proof_query_backward_eval(Key,Temp,Head,0.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	\+proof_query_backward(Key,Temp,Body).	
*/


%proof_query_backward_eval(Key,Temp,A,0.0) :-
%	\+proof_query_backward_eval(Key,Temp,A,_).


% check evidence eval
proof_query_backward_eval(Key,true,1.0) :-
	!.

proof_query_backward_eval(Key,(A,B),W) :-
	!,
	proof_query_backward_eval(Key,A,W1),
	proof_query_backward_eval(Key,B,W2),
	W is W1*W2.

% TO CHECK
proof_query_backward_eval(Key,findall_forward(X,Y,Z),1.0) :-
	findall(X,proof_query_backward(Key,Y),Z),
	!.

proof_query_backward_eval(Key,\+A,1.0) :-
	user:builtin(A),
	!,
	\+user:A.
		
proof_query_backward_eval(Key,A,1.0) :-
%	A\=(\+_),
	user:builtin(A),
	!,
	user:A.

proof_query_backward_eval(Key,\+A,1.0) :-
	\+proof_query_backward(Key,A),!.

proof_query_backward_eval(Key,A,1.0) :-
	ground(A),
	A\=(\+_),
	recorded(Key,A,_),
	A\= _~= distribution(_),
	!.

% to support non-sampled variables H ~= distribution(D) in the particles		
proof_query_backward_eval(Key,H~=Val,W) :-
	ground(H~=Val),
	recorded(Key,H ~= distribution(D),R),
	likelihood_weighting(Val,D,W),
	erase(R),
	recorda(Key,H~=Val,_),
	!.
	
%%% Tabling %%%

proof_query_backward_eval(Key,A,1.0) :-
	A\=(\+_),
	recorded(Key,A,_),
	A\= _~= distribution(_).

proof_query_backward_eval(Key,H ~= S,1.0) :-
	\+ground(S),
	recorded(Key,H ~= distribution(D),R),
	\+recorded(Temp,H ~= _,_),
	sample(D,Val),
	erase(R),
	recorda(Key,H ~= Val,_),
	S=Val.
		
proof_query_backward_eval(Key,Head ~= Var,W) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	proof_query_backward(Key,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Key,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Key,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_eval(Key,Head ~= Var,W) :-
	recorded(global,distributionalclause(Head,Distribution,Body,_),_),
	(
	 	proof_query_backward(Key,Body),
		%ground(Head),
		ground(Distribution),
		%ground(Body),
		(
			ground(Head ~= Var) ->
			(
				\+recorded(Key,Head ~= _,_),
				likelihood_weighting(Var,Distribution,W),
				recorda(Key,Head ~= Var,_)
			)
			;
			(
				\+recorded(Key,Head ~= _,_),
				sample(Distribution,Var),
				recorda(Key,Head ~= Var,_),
				W=1.0
			)
		)
	).

proof_query_backward_eval(Key,Head,1.0) :-
	Head\= _ ~= _,
	user:hardclause(Head,Body,_),
	proof_query_backward(Key,Body),
	ground(Head),
	\+recorded(Key,Head,_),
	recorda(Key,Head,_).

% end check evidence eval

check_evidence_backward(Key,Wtot) :-
	bb_put(wevidence,1.0),
	(	% check positive evidence
		user:evidence(H,1),
		bb_get(wevidence,Wold),
		check_evidence_proof(Key,H,W),%proof_query_backward_eval(Key,Key,H,W),
		Wnew is Wold*W,
		bb_put(wevidence,Wnew),
		fail;
		true
	),
	bb_delete(wevidence,Wtot).

% do not remove
check_evidence_proof(Key,H,W) :-
	proof_query_backward_eval(Key,H,W),
	%writeln(proof_query_backward_eval(Key,Key,H,W)),
	!.

log_likelihood_weighting(Val,D,LogW) :-
	likelihood_weighting(Val,D,W),
	LogW is log(W),!.

log_likelihood_weighting(Val,logfinite(L),W) :- % log weight
	member(W:Val,L),!.

log_likelihood_weighting(Val,logfinite(L),(-inf)) :- % log weight
	\+member(W:Val,L),!.

likelihood_weighting(Val,uniform(L),W) :-
	memberchk(Val,L),
	length(L,N),
	W is 1/N,!.

likelihood_weighting(Val,uniform(L),0.0) :-
	\+memberchk(Val,L),
	!.

likelihood_weighting(Val,contUniform(A,B),W) :-
	Val>=A,
	Val=<B,
	W is 1/(B-A),!.

likelihood_weighting(Val,contUniform(A,B),0.0) :-
	(Val<A;Val>B),!.
	
likelihood_weighting(Val,finite(L),W) :-
	member(W:Val,L),!.

likelihood_weighting(Val,finite(L),0.0) :-
	\+member(W:Val,L),!.
/*
likelihood_weighting(Val,logfinite(L),W) :- % log weight
	member(W:Val,L),!.

likelihood_weighting(Val,logfinite(L),(-inf)) :- % log weight
	\+member(W:Val,L),!.
*/
likelihood_weighting(Val,gaussian(M,Cov),W) :-
	test_to_list(Val,List),
	is_list(M),
	is_list(Cov),
	densityGaussian(M,Cov,List,W),!.

likelihood_weighting(Val,gaussian(M,Cov),W) :-
	test_to_list(Val,List),
	\+is_list(M),
	\+is_list(Cov),
	densityGaussian([M],[Cov],List,W),!.

likelihood_weighting(Val,student(Nu,Mean,Var),W) :-
	X is (Val-Mean)/sqrt(Var),
	studentPdf(Nu,X,W),!.

listnelem([],0) :- !.
listnelem([A|T],L) :-
	L>0,
	L1 is L-1,
	listnelem(T,L1), !.

likelihood_weighting(Val,indepGaussians([(M,Cov)|T]),W) :-
	test_to_list(Val,List),
	is_list(M),
	is_list(Cov),
	length(M,Len),
	listnelem(NL,Len),
	append(NL,Suff,List),
	densityGaussian(M,Cov,NL,W1),
	length(Suff,Res),
	(
	Res>0 ->
		(
		test_to_list(Val2,Suff),
		likelihood_weighting(Val2,indepGaussians(T),W2),
		W is W1*W2
		)
	;
		W=W1
	),
	!.
% complete likelihood_weighting

%%%



% findall for the forward chaining
user:builtin(findall_forward(_,_,_)).
%findall_forward(X,Y,Z) :-
%	findall(X,query_proof(Y),Z).


% R is between the cumulative C
cumul([H|_],R,V,C) :- 
	H = C:V, 
	R=<C,
	!.
	
cumul([H|T],R,Val,C) :-
	H = P:_, 
	R2 is R-P, 
	cumul(T,R2,Val,C2),
	C is C2+P.

%TO TEST R is between the cumulative C
logcumul([H|_],R,V,C) :- 
	H = C:V, 
	R=<exp(C),
	!.
	
logcumul([H|T],R,Val,C) :-
	H = P:_, 
	R2 is R-exp(P), 
	logcumul(T,R2,Val,C2),
	C is C2+exp(P).

exactsampling(finite(Distribution),Val,P) :-
	!,
	member(P:Val,Distribution).

exactsampling(uniform(L),Val,W) :-
	!,
	member(Val,L),
	length(L,N),
	W is 1/N.

exactsampling(D,Val,0.1) :-
	between(1,10,I),
	sample(D,Val).
	
% sample a value from a given distribution
sample(finite(Distribution),Val) :-
	X is random,
	Distribution\=[],
	cumul(Distribution,X,Val,_), !.

sample(logfinite(Distribution),Val) :-
	X is random,
	Distribution\=[],
	logcumul(Distribution,X,Val,_),!.

sample(val(Val),Val) :-
	!.
	
sample(uniform([Val]),Val) :-
	!.

sample(uniform(Distribution),Val) :-
	Distribution\=[],
	draw_uniform(Distribution,Val), !.

sample(beta(Alpha,Beta),Val) :-
	dirichlet([Alpha,Beta],Val), !.

sample(student(Nu),Val) :-
	student(Nu,Val), !.

sample(student(Nu,Mean,Var),Val) :-
	student(Nu,StVal),
	Val is StVal*sqrt(Var)+Mean, !.

sample(gamma(Alpha,Beta),Val) :-
	gamma(Alpha,Beta,Val), !.

sample(invgamma(Alpha,Beta),Val) :-
	B is 1/Beta,
	gamma(Alpha,B,Precision),
	Val is 1.0/Precision,!.
			
% sample continuous and uniform distribution
sample(contUniform(A,B),Val) :-
	sample_uniform(A,B,Val),
	 !.

sample(contUniform([(A,B)]),Val) :-
	sample_uniform(A,B,Val),
	 !.
	  
sample(contUniform([(A,B)|T]),(Val,Val2)) :-
	sample(contUniform(T),Val2),
	sample_uniform(A,B,Val),
	 !.

sample(contUniform(A,B,C,D),(Val,Val2)) :-
	sample_uniform(A,B,Val),
	sample_uniform(C,D,Val2),
	 !.

sample(contUniform(A,B,C,D,E,F),(Val,Val2,Val3)) :-
	sample_uniform(A,B,Val),
	sample_uniform(C,D,Val2),
	sample_uniform(E,F,Val3),
	 !.
	  
sample(contUniform(A,B,C,D,E,F,G,H),(Val,Val2,Val3,Val4)) :-
	sample_uniform(A,B,Val),
	sample_uniform(C,D,Val2),
	sample_uniform(E,F,Val3),
	sample_uniform(G,H,Val4),
	 !.

sample(optimalProposal(FX,SigmaV,C,SigmaW,Y),Tuple) :-
	optimalproposal(FX,SigmaV,C,SigmaW,Y,Val,W), % args: 1: f(x_{t-1}), 2: sigma_v, 3: C, 4: sigma_w, 5: y, 6: sampled state, 7: weight
	test_to_list(Tuple,Val),
	!.

sample(indepOptimalProposals(List),Tuple) :-
	indepOptimalProposals(List,Val,W),
	test_to_list(Tuple,Val),
	!.

indepOptimalProposals([(FX,SigmaV,C,SigmaW,Y)],Val,W) :-
	length(Y,Ny),findall(0,between(1,Ny,XX),MeanMeas),
	kalmanrao_simplified(FX,SigmaV,C,MeanMeas,SigmaW,Y,Mpost,CovPost,W),
	sample(gaussian(Mpost,CovPost),Tuple),
	test_to_list(Tuple,Val),
%	optimalproposal(FX,SigmaV,C,SigmaW,Y,Val,W),
	!.
	
indepOptimalProposals([(FX,SigmaV,C,SigmaW,Y)|T],Ris,W) :-
	indepOptimalProposals(T,SubList,WT),
%	optimalproposal(FX,SigmaV,C,SigmaW,Y,Val,WH),
	length(Y,Ny),findall(0,between(1,Ny,XX),MeanMeas),
	kalmanrao_simplified(FX,SigmaV,C,MeanMeas,SigmaW,Y,Mpost,CovPost,WH),
	sample(gaussian(Mpost,CovPost),Tuple),
	test_to_list(Tuple,Val),
	W is WT*WH,
	list_concat([Val,SubList],Ris),
	!.

sample(logIndepOptimalProposals(List),[Tuple,W]) :-
	logIndepOptimalProposals(List,Val,W),
	test_to_list(Tuple,Val),
	!.

logIndepOptimalProposals([(FX,SigmaV,C,SigmaW,Y)],Val,LogW) :-
	length(Y,Ny),findall(0,between(1,Ny,XX),MeanMeas),
	kalmanrao_simplified(FX,SigmaV,C,MeanMeas,SigmaW,Y,Mpost,CovPost,W),
	sample(gaussian(Mpost,CovPost),Tuple),
	test_to_list(Tuple,Val),
	LogW is log(W),!.
	
logIndepOptimalProposals([(FX,SigmaV,C,SigmaW,Y)|T],Ris,W) :-
	logIndepOptimalProposals(T,SubList,WT),
%	optimalproposal(FX,SigmaV,C,SigmaW,Y,Val,WH),
	length(Y,Ny),findall(0,between(1,Ny,XX),MeanMeas),
	kalmanrao_simplified(FX,SigmaV,C,MeanMeas,SigmaW,Y,Mpost,CovPost,WH),
	sample(gaussian(Mpost,CovPost),Tuple),
	test_to_list(Tuple,Val),
	W is WT+log(WH),
%	writeln(W is WT+log(WH)),
	list_concat([Val,SubList],Ris),!.

sample(gaussian([Hm|Mean],[Hc|Cov]),Tuple) :-
	!,
	gaussian([Hm|Mean],[Hc|Cov],Val),
	test_to_list(Tuple,Val),
	!.
	
sample(gaussian(Mean,Var),Val) :-
	gaussian([Mean],[Var],[Val]),
	 !.

checkline([C],[],0,C) :-
	!.
	
checkline([C1|H],[A|B],X,C) :-
	checkline(H,B,X2,C),
	X is X2+C1*A.

% Test open('data.txt','write',S),findall(A,(between(1,1000,I),distributionalclause:sample(gaussian_cutmax([0],[0.1],(1,0.1)),A),write(S,A),write(S,' '),write(S,1),nl(S)),_),close(S).
% open('data.txt','write',S),findall(A,(between(1,1000,I),distributionalclause:sample(gaussian_cutmax([0,0],[0.1,0,0,0.1],(1,1,0.0)),(A,B)),write(S,A),write(S,' '),write(S,B),nl(S)),_),close(S).
sample(gaussian_cutmax([Hm|Mean],[Hc|Cov],Limit1),Tuple) :-
	!,
	gaussian([Hm|Mean],[Hc|Cov],Val),
	test_to_list(Limit1,Limit),
	checkline(Limit,Val,X,C),
	(
		X>C ->
			sample(gaussian_cutmax([Hm|Mean],[Hc|Cov],Limit1),Tuple)
		;
			test_to_list(Tuple,Val)
	),!.

sample(gaussian_cutmin([Hm|Mean],[Hc|Cov],Limit1),Tuple) :-
	!,
	gaussian([Hm|Mean],[Hc|Cov],Val),
	test_to_list(Limit1,Limit),
	checkline(Limit,Val,X,C),
	(
		X<C ->
			sample(gaussian_cutmin([Hm|Mean],[Hc|Cov],Limit1),Tuple)
		;
			test_to_list(Tuple,Val)
	),!.
	
sample(gaussian_cutmax(Mean,Var,Limit),Val) :-
	gaussian([Mean],[Var],[Val1]),
	Val is min(Val1,Limit),
	!.

sample(gaussian_cutmin(Mean,Var,Limit),Val) :-
	gaussian([Mean],[Var],[Val1]),
	Val is max(Val1,Limit),
	!.
	 
sample(dirichlet(A),List) :-
	dirichlet(A,List),
	 !.

% sample couples (X,Y) from 2 independent gaussians
sample(indepGaussians(List),Tuple) :-
	indepGaussians(List,Val),
	test_to_list(Tuple,Val),
	!.

indepGaussians([(M,C)],Val) :-
	gaussian(M,C,Val),
	!.
	
indepGaussians([(M,C)|T],Ris) :-
	indepGaussians(T,SubList),
	gaussian(M,C,Val),
	list_concat([Val,SubList],Ris),
	!.
	
sample(indepGaussians_cutmin([(M,C)],[Limit]),[Val]) :-
	sample(gaussian_cutmin(M,C,Limit),Val),!.


sample(indepGaussians_cutmin([(M,C)|T],[Limit|H]),Ris) :-
	sample(indepGaussians_cutmin(T,H),SubList),
	sample(gaussian_cutmin(M,C,Limit),Val),
	list_concat([[Val],SubList],Ris),!.
	

sample(indepGaussians_cutmax([(M,C)],[Limit]),[Val]) :-
	sample(gaussian_cutmax(M,C,Limit),Val),!.


sample(indepGaussians_cutmax([(M,C)|T],[Limit|H]),Ris) :-
	sample(indepGaussians_cutmax(T,H),SubList),
	sample(gaussian_cutmax(M,C,Limit),Val),
	list_concat([[Val],SubList],Ris),!.

/*	
sample(gaussian(MeanX,VarX,MeanY,VarY),(ValX,ValY)) :-
	normal2(MeanX,VarX,MeanY,VarY,ValX,ValY),
	 !.

% sample 4 values from 4 independent gaussians	
sample(gaussian(Mean1,Var1,Mean2,Var2,Mean3,Var3,Mean4,Var4),(Val1,Val2,Val3,Val4)) :-
	normal(Mean1,Var1,Val1),normal(Mean2,Var2,Val2),
	normal(Mean3,Var3,Val3),normal(Mean4,Var4,Val4),
	 !.
*/
sample(poisson(L),Val) :-
	poisson(L,Val), !.


%%%%%% magic set %%%%%%%%%%

% Magic transformation
magic :-
	retractall(user:hardclause(_,_,_)),
	retractall(user:distributionalclause(_,_,_,_)),
	(
		get_magic(true)
		->
			(
			%retractall(user:hardclause(_,_,_)),
			%retractall(user:distributionalclause(_,_,_,_)),
			magic_hardclause,
			magic_distributionalclause
			)
		;
		(
			get_magic(particle)
			->
			(
				expansion_magic_off
				%magic_particlefilter
			)
			;
				expansion_magic_off
		)
	).

timesyntax(H:t,current(H)) :-
	!.

timesyntax(findall_forward(A,B,C),findall_forward(A,BB,C)) :-
	timesyntax(B,BB),
	!.


timesyntax(H:t+1,next(H)) :-
	!.

timesyntax(H:0,prior(H)) :-
	!.

timesyntax(H:t ~= V,current(H) ~= V) :-
	!.
	
timesyntax(H:0 ~= V,prior(H) ~= V) :-
	!.
	
timesyntax(H:t+1 ~= V,next(H) ~= V) :-
	!.

timesyntax((H,H2),(HH,HH2)) :-
	timesyntax(H,HH),
	timesyntax(H2,HH2),
	!.
timesyntax(\+H,\+HH) :-
	timesyntax(H,HH),
	!.

timesyntax(H,H) :-
	H\= (_,_),
	H\= _:0,
	H\= _:t,
	H\= _:t+1,
	H\= _:t ~= _,
	H\= _:t+1 ~= _,
	!.

expansion_magic_off :-
	(
		user:(H ~ D),
		H\= _:_,
		assert(user:distributionalclause(H,D,true,0)),
		fail;
		true
	),
	(
		user:(H:0 ~ D),
		assert(user:distributionalclause(prior(H),D,true,0)),
		fail;
		true
	),
	(
		user:(H:t ~ D),
		assert(user:distributionalclause(current(H),D,true,0)),
		fail;
		true
	),
	(
		user:(H:t+1 ~ D),
		assert(user:distributionalclause(next(H),D,true,1)),
		fail;
		true
	),
	
	(
		user:(H ~ D:=B),
		H\= _:_,
		assert(user:distributionalclause(H,D,B,0)),
		fail;
		true
	),
	(
		user:(H:0 ~ D:=B),
		timesyntax(B,BB),
		assert(user:distributionalclause(prior(H),D,BB,0)),
		fail;
		true
	),
	(
		user:(H:t ~ D:=B),
		timesyntax(B,BB),
		assert(user:distributionalclause(current(H),D,BB,0)),
		fail;
		true
	),
	(
		user:(H:t+1 ~ D:=B),
		timesyntax(B,BB),
		assert(user:distributionalclause(next(H),D,BB,1)),
		fail;
		true
	),
	
	(
		user:(H := B),
		H\=_~_,
		H\=_:_,
		assert(user:hardclause(H,B,0)),
		fail;
		true
	),
	(
		user:(H:0 :=B),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(prior(H),BB,0)),
		fail;
		true
	),
	(
		user:(H:t :=B),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(current(H),BB,0)),
		fail;
		true
	),
	(
		user:(H:t+1 :=B),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(next(H),BB,1)),
		fail;
		true
	),
	
	(
	
		user:(H ~ D pr X),
		H\=_:_,
		assert(user:distributionalclause(H,D,true,X)),
		fail;
		true
	),
	(
	
		user:(H:0 ~ D pr X),
		assert(user:distributionalclause(prior(H),D,true,X)),
		fail;
		true
	),
	(
	
		user:(H:t ~ D pr X),
		assert(user:distributionalclause(current(H),D,true,X)),
		fail;
		true
	),
	(
	
		user:(H:t+1 ~ D pr X),
		assert(user:distributionalclause(next(H),D,true,X)),
		fail;
		true
	),
	
	(
		user:(H ~ D := B pr X),
		H\=_:_,
		assert(user:distributionalclause(H,D,B,X)),
		fail;
		true
	),
	(
		user:(H:0 ~D:=B pr X),
		timesyntax(B,BB),
		assert(user:distributionalclause(prior(H),D,BB,X)),
		fail;
		true
	),
	(
		user:(H:t ~D:=B pr X),
		timesyntax(B,BB),
		assert(user:distributionalclause(current(H),D,BB,X)),
		fail;
		true
	),
	(
		user:(H:t+1 ~D:=B pr X),
		timesyntax(B,BB),
		assert(user:distributionalclause(next(H),D,BB,X)),
		fail;
		true
	),
	
	(
		user:(H := B pr X),
		H\=_~_,
		H\=_:_,
		assert(user:hardclause(H,B,X)),
		fail;
		true
	),
	(
		user:(H:0 :=B pr X),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(prior(H),BB,X)),
		fail;
		true
	),
	(
		user:(H:t :=B pr X),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(current(H),BB,X)),
		fail;
		true
	),
	(
		user:(H:t+1 :=B pr X),
		H\=_~_,
		timesyntax(B,BB),
		assert(user:hardclause(next(H),BB,X)),
		fail;
		true
	).


add_call(A ~= _,callmagic(dist_eq(A))) :-
	!.
add_call(\+(A ~= _),callmagic(dist_eq(A))) :-
	!.

add_call(A,callmagic(A)) :-
	 A \= (\+Something),
	 !.

add_call(A,callmagic(Something)) :-
	 A = (\+Something),
	 !.

/*
add_call(A,Call_A) :-
	A=..[F|Arg],
	atomic_concat(['call_',F],F2),
	Call_A=..[F2|Arg].
*/
split_body(Body,T,Prefix) :-
	test_to_list(Body,L),
	append(Part1,[T|_],L),
	test_to_list(Prefix,Part1).


% convert from (A,B,C...) to [A,B,C,...]
test_to_list(true,[]) :-
	!.
test_to_list(A,[A]) :-
	\+ A=(_,_),
	!.
test_to_list((A,T1),[A|T2]) :-
	test_to_list(T1,T2).

magic_hardclause(Head,Body,Pr) :-
	test_to_list(Body,B),
	findManage(B,NewB),
	magic_set_hard(Head,NewB,0),
	(

		add_call(Head,Head_Call),
		(
			Body == true
			->
				NewBody=Head_Call
				;
				NewBody=(Head_Call,Body)
		)
			
	),
	%write((Head := NewBody pr Pr)),nl,
	assert(user:hardclause(Head,NewBody,Pr)).

magic_distributionalclause(Head,D,Body,Pr) :-
	(
		add_call(Head ~= X,Head_Call),
		(
			Body == true
			->
				NewBody=Head_Call;
				NewBody=(Head_Call,Body)
		)
		
	),
	%write((Head ~ D := NewBody pr Pr)),nl,
	assert(user:distributionalclause(Head,D,NewBody,Pr)),
	test_to_list(Body,B),
	findManage(B,NewB),
	magic_set_hard(Head ~= X,NewB,0).

% TO ASK! NOT FINISHED findall forward with magic (Temporal solution)
findManage(Body,NewBody) :-
	%test_to_list(B,Body),
	(
		append(P,[findall_forward(A,T,C)|Tail],Body)
		->
		(
			copy_term(T,T2),
			test_to_list(T2,ListT),
			append(P,ListT,P2),
			append(P2,Tail,NewBody1),% remove findall_forward append(P2,[findall_forward(A,T,C)|Tail],NewBody)
			remove_builtin(NewBody1,NewBody) % remove builtin TO CHECK!
		);
		NewBody=Body
	).
	%test_to_list(NewBody,NewBody1).

magic_hardclause :-
	(
		user:(Head := Body),
		Head\=V~D,
		%write((Head := Body)),nl,
		
		magic_hardclause(Head,Body,0),
		%nl,
		fail;
		true
	),
	(
		user:(Head := Body pr X),
		Head\=V~D,
		%write((Head := Body pr X)),nl,
		
		magic_hardclause(Head,Body,X),
		%nl,
		fail;
		true
	).
	
magic_distributionalclause :-
	(
		user:(Head ~ D := Body),
		%write((Head ~ D := Body)),nl,
		
		magic_distributionalclause(Head,D,Body,0),
		%nl,
		fail;
		true
	),
	(
		user:(Head ~ D),
		%write((Head ~ D)),nl,
		magic_distributionalclause(Head,D,true,0),
		%nl,
		fail;
		true
	),
	(
		user:(Head ~ D := Body pr X),
		%write((Head ~ D := Body pr X)),nl,
		
		magic_distributionalclause(Head,D,Body,X),
		%nl,
		fail;
		true
	),
	(
		user:(Head ~ D pr X),
		%write((Head ~ D pr X)),nl,
		magic_distributionalclause(Head,D,true,X),
		%nl,
		fail;
		true
	).




remove_builtin([true],[true]) :-
	!.

remove_builtin([H],[]) :-
	user:builtin(H),
	!.

remove_builtin([H],[H]) :-
	\+user:builtin(H),
	!.

remove_builtin([H|T],NewBody) :-
	(
		user:builtin(H)
		->
		(
			remove_builtin(T,NewBody)

		)
		;
		(
			remove_builtin(T,NewT),
			NewBody=[H|NewT]
		)
	).

magic_set_hard(Head,[],Pr) :-
	!.

magic_set_hard(Head,Body,Pr) :-
	(
		append(Prefix,[Last|[]],Body),
		(
			(user:builtin(Last))%;remove_magic(Last))
			->
			true
			;
			(
				add_call(Last,Call_Last),
				(
					%remove_magic(Head) ->
					%	test_to_list(NewBody,Prefix)
					%;
						(
							add_call(Head,Call_Head),
							test_to_list(NewBody,[Call_Head|Prefix])
						)
				),
				%write((Call_Last := NewBody)),nl,
				assert(user:hardclause(Call_Last,NewBody,Pr))
			)
		)
	),
	magic_set_hard(Head,Prefix,Pr),
	!.


init_query(Key,_) :-
	get_magic(false),
	!.
init_query(Key,(H;T)) :-
	!,
	init_query(Key,H),
	init_query(Key,T).
init_query(Key,(H,T)) :-
	!,
	init_query(Key,H),
	init_query(Key,T).
init_query(Key,\+ Atom) :-
	!,
	add_call(Atom,CallAtom),
	recorda(Key,CallAtom,_).
	
init_query(Key,Atom) :-
	add_call(Atom,CallAtom),
	recorda(Key,CallAtom,_).


% add callmagic for the evidence
init_query_list(Key,[]).

init_query_list(Key,_) :-
	get_magic(false),
	!.

init_query_list(Key,[H|T]) :-
	init_query(Key,H),
	init_query_list(Key,T).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

assert_evidence([],V).

assert_evidence([H|T],V) :-
	assert(user:evidence(H,V)),
	assert_evidence(T,V).

assert_impossible_evidence(Key,[]).

assert_impossible_evidence(Key,[H|T]) :-
	recorda(Key,H,_).	
	
/*
assert_evidence(true) :-
	!.
assert_evidence((A,B)) :-
	!,
	assert_evidence(A),
	assert_evidence(B).

assert_evidence(\+H) :-
	assert(user:evidence(H,0)).

assert_evidence(H) :-
	assert(user:evidence(H,1)).
*/

% Sum = Distr1 + Distr2 * Weight
sum_distrib(finite(Distr1),finite(Distr2),Weight,finite(Sum)) :-
	findall(W:Val,(member(W1:Val,Distr1),member(W2:Val,Distr2),W is W1+W2*Weight),NewDist),
	%write(NewDist),nl,
	findall(W:Val,(member(W:Val,Distr1),\+member(_:Val,Distr2)),NewDist2),
	%write(NewDist2),nl,
	findall(W:Val,(member(W2:Val,Distr2), W is W2*Weight,\+member(_:Val,Distr1)),NewDist3),
	%write(NewDist3),nl,
	append(NewDist,NewDist2,TOT),
	append(NewDist3,TOT,Sum).


sum_list_multidim(Distr1,Distr2,Sum) :-
	findall(W:Val,(member(W1:Val,Distr1),member(W2:Val,Distr2),sum_multidim(W1,W2,W)),NewDist),
	%write(NewDist),nl,
	findall(W:Val,(member(W:Val,Distr1),\+member(_:Val,Distr2)),NewDist2),
	%write(NewDist2),nl,
	findall(W:Val,(member(W:Val,Distr2),\+member(_:Val,Distr1)),NewDist3),
	%write(NewDist3),nl,
	append(NewDist,NewDist2,TOT),
	append(NewDist3,TOT,Sum).
			
% sum of probabilities in a finite distribution
sum_prob([P:V],P) :-
	!.

sum_prob([P:V|T],Sum) :-
	sum_prob(T,Sum2),
	Sum is Sum2+P.

sum_multidim(A,B,Sum) :-
	A\=(_,_),
	B\=(_,_),
	Sum is A+B,
	!.

sum_multidim((A,B),(C,D),(Sum,Sum2)) :-
	sum_multidim(B,D,Sum2),
	Sum is A+C.


div_scalar_multidim(P,Tot,P2) :-
	P\=(_,_),
	P2 is P/Tot,
	!.
	
div_scalar_multidim((P,Tail),Tot,(P2,NewDistrTail)) :-
	P2 is P/Tot,
	div_scalar_multidim(Tail,Tot,NewDistrTail),
	!.

prod_scalar_multidim(P,Tot,P2) :-
	P\=(_,_),
	P2 is P*Tot,
	!.
	
prod_scalar_multidim((P,Tail),Tot,(P2,NewDistrTail)) :-
	P2 is P*Tot,
	prod_scalar_multidim(Tail,Tot,NewDistrTail),
	!.
		
divide_multidim(Distr1,Distr2,NewDist) :-
	findall(W:Val,(member(W1:Val,Distr1),member(W2:Val,Distr2),div_scalar_multidim(W1,W2,W)),NewDist).

% products element of a list
product_list([],1) :-
	!.
	
product_list([H|T],P) :-
	product_list(T,SubP),
	P is SubP*H.
	

% divide probabilities by Tot in a finite distribution
divideby([P:V],Tot,NewDistr) :-
	P2 is P/Tot,
	NewDistr=[P2:V],
	!.
	
divideby([P:V|Tail],Tot,NewDistr) :-
	P2 is P/Tot,
	divideby(Tail,Tot,NewDistrTail),
	NewDistr=[P2:V|NewDistrTail],
	!.

%(clean distribution removing values with prob. 0)

cleanDistribution([],[],_) :- !.
	
cleanDistribution([P:V|Tail],NewDistr,Limit) :-
	cleanDistribution(Tail,NewDistrTail,Limit),
	(
		P>Limit ->
		NewDistr=[P:V|NewDistrTail]
		;
		NewDistr=NewDistrTail
	).

findmax(P:V,[P:V]) :- !.
findmax(PM:M,[PH:H|T]) :-
	findmax(PMT:MT,T),
	(
		PMT>PH ->
			(
			PM=PMT,
			M=MT
			)
		;
		(
			PM=PH,
			M=H
		)
	).
	
% multiply probabilities by W in a finite distribution
%(clean distribution removing values with prob. 0)
multiplyby([P:V],W,NewDistr) :-
	P2 is P*W,
	(
		P2>0 ->
		NewDistr=[P2:V]
		;
		NewDistr=[]
	),
	!.
	
multiplyby([P:V|Tail],W,NewDistr) :-
	P2 is P*W,
	multiplyby(Tail,W,NewDistrTail),
	(
		P2>0 ->
		NewDistr=[P2:V|NewDistrTail]
		;
		NewDistr=NewDistrTail
	),	
	!.

% normalize a finite distribution
normalize([],[]) :- !.

normalize(Distr,DistrNorm) :-
	%write(Distr),nl,
	sum_prob(Distr,Sum),
	divideby(Distr,Sum,DistrNorm).

sample_lookahead(Key,Head,poisson(A),poisson(A),1.0) :- 
	writeln('warning! not implemented'),!.
% TO CHECK! NOT VERIFIED
sample_lookahead(Key,Head,gaussian(A,B),Distr,W) :- 
	\+is_list(A),
	\+is_list(B),
	!,
	(
		(user:evidence(Head ~= V,1), ground(V))
		->
		(
			Distr=uniform([V]),
			%W is 1/(sqrt(2*pi*B))*exp(-(X-A)*(X-A)/(2*B)) % TO CHECK! NOT VERIFIED
			densityGaussian([A],[B],[V],W),
			writeln('warning! not tested')
		)
		;
		(
			Distr=gaussian(A,B),
			W is 1.0
		)
	),
	!.

sample_lookahead(Key,Head,contUniform(A,B),Distr,1.0) :- 
	!,
	(
		(user:evidence(Head ~= V,1), ground(V))
		->
		(
			Distr=uniform([V]),
			writeln('warning! not tested')
		)
		;
		Distr=contUniform(A,B)
	),
	!.	
	
% TO CHECK! NOT VERIFIED
/*
sample_lookahead(Key,Head,uniform(A,B,C,D),Distr,1.0) :- 
	!,
	(
		(user:evidence(Head ~= V,1), ground(V))
		->
		(
			Distr=uniform([V])
		)
		;
		Distr=uniform(A,B,C,D)
	),
	!.	
*/

sample_lookahead(Key,Head,bigaussian((A,B),(C,D,E,F)),bigaussian((A,B),(C,D,E,F)),1.0) :-
	writeln('warning! not tested'),!.
sample_lookahead(Key,Head,dirichlet(A),dirichlet(A),1.0) :-
	writeln('warning! not tested'),!.
sample_lookahead(Key,Head,gaussian(A,B,C,D),gaussian(A,B,C,D),1.0) :-
	writeln('warning! not tested'),!.
	
sample_lookahead(Key,Head,gaussian(A,B),gaussian(A,B),1.0) :-
	writeln('warning! not tested'),
	is_list(A),
	is_list(B),!.
	
sample_lookahead(Key,Head,uniform(A,B,C,D),uniform(A,B,C,D),1.0) :-
	!.

sample_lookahead(Key,Head,gaussian(A,B,C,D,E,F,G,H),gaussian(A,B,C,D,E,F,G,H),1.0) :-
	writeln('warning! not tested'),!.
sample_lookahead(Key,Head,uniform(A,B,C,D,E,F,G,H),uniform(A,B,C,D,E,F,G,H),1.0) :-
	!.
	
sample_lookahead(Key,Head,uniform(Distribution),uniform(NewDistr),Weight) :-
	!,
	(
		user:evidence(Head ~= V,1)
		->
		(
			ground(V)
			->
			(
				NewDistr=[V],
				length(Distribution,S),
				Weight is 1/S
			);
			(
				Weight = 1.0,
				NewDistr = Distribution
			)
		)
		;
		(
			bb_put(distr,Distribution),
			(
				user:evidence(Head ~= V,0),
				bb_get(distr,D),
				delete(D,V,D2), % remove false evidence from distribution
				bb_put(distr,D2),
				fail;
				true
			),
			bb_delete(distr,Dist),
			remove_inconsistent_value(Key,Head,uniform(Dist),NewDistr,SumRemoved,3),
			
			length(NewDistr,Snew),
			length(Distribution,Sold),
			Weight is Snew/Sold
		)
	),
	!.
	
sample_lookahead(Key,Head,finite(Distribution),finite(NewDistr),Weight) :-
	!,
	(
		user:evidence(Head ~= V,1)
		->
		(
			ground(V)
			->
			(
				NewDistr=[1:V],
				member(Weight:V,Distribution)
				% Weight is P
			);
			(
				Weight = 1.0,
				NewDistr = Distribution
			)
		)
		;
		(
			bb_put(distr,Distribution),
			bb_put(w,1.0),
			(	% check false evidence
				user:evidence(Head ~= V,0),
				bb_get(distr,D),
				member(Prob:V,D),
				delete(D,Prob:V,D2), % remove false evidence from distribution
				bb_put(distr,D2),
				bb_get(w,W1),
				W2 is W1-Prob, % update weight
				bb_put(w,W2),
				fail;
				true
			),
			bb_delete(distr,D3),
			remove_inconsistent_value(Key,Head,finite(D3),NewDistrNotNorm,SumP,3),
			normalize(NewDistrNotNorm,NewDistr),
			bb_delete(w,W4),
			Weight is W4-SumP
		)
	),
	!.



%%% evidence proof
evidence_proof_exists_maybe(Key,Depth) :-
	\+ (user:evidence(Atom,1), \+ proof_exists_maybe(Key,Atom,Depth)).

proof_exists_maybe(Key,true,_Depth) :-
	!.
	
proof_exists_maybe(Key,(A,B),Depth) :-
	!,
	proof_exists_maybe(Key,A,Depth),
	proof_exists_maybe(Key,B,Depth).

/* TO CHECK
proof_exists_maybe(A is integer(B),_Depth) :-
	!,
	integer(A),
	B=A,
	!.
*/
proof_exists_maybe(Key,between(A,B,C),_Depth) :-
	\+ground(B),
	A=<C,
	!.

proof_exists_maybe(Key,A is B,_Depth) :-
	\+ground(B),
	!.

proof_exists_maybe(Key,callmagic(_),_Depth) :-
	!.

% TO CHECK
proof_exists_maybe(Key,findall_forward(X,Y,Z),Depth) :-
	findall(X,proof_exists_maybe(Key,Y,0),Z),
	!.

% TO CHECK
proof_exists_maybe(Key,min_list(A,B),_Depth) :-
	\+ground(A),
	!.
proof_exists_maybe(Key,max_list(A,B),_Depth) :-
	\+ground(A),
	!.
	
proof_exists_maybe(Key,A,_Depth) :-
	user:builtin(A),
	!,
	user:A.

% TO CHECK
proof_exists_maybe(Key,\+A,_Depth) :-
	\+recorded(Key,A,_).
	
proof_exists_maybe(Key,A,_Depth) :-
	recorded(Key,A,_).

proof_exists_maybe(Key,Head ~= Var,Depth) :-
	user:distributionalclause(Head,Distribution,Body,_),
	(
	 	Distribution=finite(L)
		->
		 (
			  \+recorded(Key,Head ~= Var1,_),
			  %copy_term(Var,Var2), % to avoid unification
			  member(_:Var,L)%memberchk(_:Var2,L) % member(_:Var,L)
		 );
		 (
			 Distribution=uniform(L)
			 ->
			 (
				\+recorded(Key,Head ~= Var1,_),
				%copy_term(Var,Var2), % to avoid unification
				member(Var,L)%memberchk(Var2,L) % member(Var,L)
			 );
			 true
		 )
	),
	Depth2 is Depth-1,
	(
		Depth2<1
		->
	 	true;
		(
			%write(Body),nl,
			proof_exists_maybe(Key,Body,Depth2)	
		)
	).
	
proof_exists_maybe(Key,A,Depth) :-
	user:hardclause(A,Body,_),
	\+recorded(Key,A,_),
	Depth2 is Depth-1,
	(
		Depth2<1
		->
		true;
		(
			%write(Body),nl,
			proof_exists_maybe(Key,Body,Depth2)	
		)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	

remove_inconsistent_value(Key,Head,finite([]),[],0.0,Depth).
remove_inconsistent_value(Key,Head,uniform([]),[],0,Depth).

remove_inconsistent_value(Key,Head,finite([P:V|H]),NewDistr,SumPremoved,Depth) :-
	(
		check_value(Key,Head ~= V,Depth)
		->
		(
			remove_inconsistent_value(Key,Head,finite(H),NewDistrH,SumPremoved1,Depth),
			NewDistr=[P:V|NewDistrH],
			SumPremoved is SumPremoved1
		);
		(
			remove_inconsistent_value(Key,Head,finite(H),NewDistrH,SumPremoved1,Depth),
			NewDistr=NewDistrH,
			SumPremoved is SumPremoved1+P
		)
	).

remove_inconsistent_value(Key,Head,uniform([V|H]),NewDistr,Nremoved,Depth) :-
	(
		check_value(Key,Head ~= V,Depth)
		->
		(
			remove_inconsistent_value(Key,Head,uniform(H),NewDistrH,Nremoved1,Depth),
			NewDistr=[V|NewDistrH],
			Nremoved is Nremoved1
		);
		(
			remove_inconsistent_value(Key,Head,uniform(H),NewDistrH,Nremoved1,Depth),
			NewDistr=NewDistrH,
			Nremoved is Nremoved1+1
		)
	).

/*
check_value(Value,Depth) :-
	recorded(sampled,Value,_),!.

check_value(H ~= V,Depth) :-
	user:evidence(H ~= V,1),!.
*/
check_value(K,H ~= V,Depth) :-
	% \+recorded(sampled,H ~= V,_),
	% \+user:evidence(H ~= V,_),
	recorda(K,H ~= V,Key),
	(
		evidence_proof_exists_maybe(K,Depth)
		->
		erase(Key);
		(
			erase(Key),
			fail
		)
	).
/*
TO CHECK
derived(Key,Head) :-
	ground(Head),
	recorded(Key,Head,_),
	!.
*/
derived(Key,Head) :-
	ground(Head),
	recorded(Key,Head,R),
	recorded(Key,H,R),
	H == Head,
	!.
	
% if Head is not grounded the check in "Key" is made without unification.
derived(Key,Head) :-
	\+ground(Head),
	copy_term(Head,Head2),
	recorded(Key,Head2,_),
	\+ \+ (numbervars(Head,0,_),numbervars(Head2,0,_),Head=Head2),
	%write(derived(Key,Head)),nl,
	!.

% compute the max priority present in the user model
get_max_priority(P) :-
	bb_put(priority,0),
	(
		
		(
			user:(hardclause(_,_,Pr)),
			bb_get(priority,PrOld),
			Pr > PrOld,
			bb_put(priority,Pr),
			fail;
			true
		),
		(
			user:(distributionalclause(_,_,_,Pr)),
			bb_get(priority,PrOld),
			Pr > PrOld,
			bb_put(priority,Pr),
			fail;
			true
		)
	),
	bb_delete(priority,P).
	


genesamplestep(Key,Weight,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		query_proof(Key,Body),
		\+derived(Key,Head),% attention if the head is not grounded! TO FIX! only callmagic allowed (maybe)
		(
			Head\=callmagic(_) ->
				ground(Head)
			;
				true
		),
		recorda(Key,Head,_),
		% write(Head),nl,
		
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	bb_put(weightstep,1.0),
	(
		%between(0,Priority,Pr), % TO CHECK
		user:distributionalclause(Head2,Distribution,Body,Pr),
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), % TO CHECK!!!
		\+recorded(Key,Head2 ~= X,_),
		(/*
			Lookahead=false
			->
				(
					NewDistr=Distribution,
					WeightStep=1.0
				)
				;*/
				sample_lookahead(Key,Head2,Distribution,NewDistr,WeightStep) % write((Head2,Distribution,NewDistr,WeightStep)),nl,
		),
		sample(NewDistr,Val),
		% update weight
		bb_get(weightstep,OldWeight),
		NewWeight is OldWeight*WeightStep,
		bb_put(weightstep,NewWeight),
		
		recorda(Key,Head2 ~= Val,_),
		%write(Head2 ~= Val),write(' | '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			bb_delete(weightstep,W1),
			Flag=true,
			genesamplestep(Key,W,Pr,_),
			Weight is W1*W
		);
		(
			bb_delete(weightstep,Weight),
			Flag=false
		)
	).

genesamplestep_all(Key,Weight,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		query_proof(Key,Body),
		\+recorded(Key,Head,_),% attention if the head is not grounded! TO FIX! only callmagic allowed (maybe)
		ground(Head),
		recorda(Key,Head,_),
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	bb_put(weightstep,1.0),
	(
		%between(0,Priority,Pr), % TO CHECK
		user:distributionalclause(Head2,Distribution,Body,Pr),
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), % TO CHECK!!!
		\+recorded(Key,Head2 ~= X,_),
		%sample_lookahead(Key,Head2,Distribution,NewDistr,WeightStep) % write((Head2,Distribution,NewDistr,WeightStep)),nl,
		finite(NewDistr)=Distribution,
		member(NewDistr,Val:WeightStep),
		% update weight
		bb_get(weightstep,OldWeight),
		NewWeight is OldWeight*WeightStep,
		bb_put(weightstep,NewWeight),
		
		recorda(Key,Head2 ~= Val,_),
		%write(Head2 ~= Val),write(' | '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			bb_delete(weightstep,W1),
			Flag=true,
			genesamplestep(Key,W,Pr,_),
			Weight is W1*W
		);
		(
			bb_delete(weightstep,Weight),
			Flag=false
		)
	).

clean_sample(Key) :-
	eraseall(Key).

% generate sample handling priority
generate_sample_pr(Key,W,MaxP) :-
	bb_put(weight3,1.0),
	(
		between(0,MaxP,Priority),
		%write('P '),write(Priority),nl,
		genesamplestep(Key,Weight,Priority,F),

		bb_get(weight3,OldWeight1),
		NewWeight1 is OldWeight1*Weight,
		%write(NewWeight1),nl,
		bb_put(weight3,NewWeight1),
		fail;
	
		true
	),
	(
		bb_delete(weight3,W)
	).
	

check_neg_evidence(Key,[]).
check_pos_evidence(Key,[]).

check_neg_evidence(Key,[Neg|T]) :-
	%\+recorded(Key,Neg,_),
	query_proof(Key,\+Neg),
	check_neg_evidence(Key,T).

check_pos_evidence(Key,[Pos|T]) :-
	%recorded(Key,Pos,_),
	query_proof(Key,Pos),
	check_pos_evidence(Key,T).

check_evidence(Key,Pos,Neg) :-
	check_pos_evidence(Key,Pos),
	check_neg_evidence(Key,Neg).


% new inference algorithm
check_neg_evidence_exp(Key,List,[]).
check_pos_evidence_exp(Key,List,[]).

check_neg_evidence_exp(Key,List,[Neg|T]) :-
	query_proof(Key,\+Neg),
	\+memberchk(Neg,List),
	check_neg_evidence_exp(Key,List,T).

check_pos_evidence_exp(Key,List,[Pos|T]) :-
	%recorded(Key,Pos,_),
	query_proof(Key,Pos),
	check_pos_evidence_exp(Key,List,T).

check_pos_evidence_exp(Key,List,[Pos|T]) :-
	%recorded(Key,Pos,_),
	memberchk(Pos,List),
	check_pos_evidence_exp(Key,List,T).

check_evidence_exp(Key,List,Pos,Neg) :-
	check_pos_evidence_exp(Key,List,Pos),
	check_neg_evidence_exp(Key,List,Neg).
	
% evaluate the query generating N samples for timestep Start to End
eval_query_step(PosEvidence,NegEvidence,Query,Start,End,N,P,Succ_Sum,Sum) :-
	magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		clean_sample(sampled),
		init_query(sampled,Query),
		init_query_list(sampled,PosEvidence),
		init_query_list(sampled,NegEvidence),
		bb_put(weight2,1.0),
		(
			between(Start,End,Timestep),
			retractall(user:timestep(_)),
			assert(user:timestep(Timestep)),
			%write('Timestep '),write(Timestep),nl,
			generate_sample_pr(sampled,Weight,MaxP),
			bb_get(weight2,OldWeight1),
			NewWeight1 is OldWeight1*Weight,
			%write(NewWeight1),nl,
			bb_put(weight2,NewWeight1),
						
			fail;
			
			true
		),
		check_evidence(sampled,PosEvidence,NegEvidence),
		bb_delete(weight2,NewWeight),
		%write(NewWeight),nl,
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + NewWeight,
		bb_put(sample_sum,New_Sum),
		(
			query_proof(sampled,Query)
			->
			(
				bb_get(succeeding_sample_sum,Old),
				New is Old+NewWeight,
				bb_put(succeeding_sample_sum,New)
			);
			true
		),
		fail;

		true
	),
	clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

% evaluate the query generating N samples
eval_query(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	(
	get_magic(backward) ->
		eval_query_backward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum)
	;
		eval_query_forward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum)
	),!.

% evaluate the query generating N samples
eval_query(ImpossibleEvidence,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	(
	get_magic(backward) ->
		eval_query_backward(ImpossibleEvidence,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum)
	;
		fail %eval_query_forward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum)
	),!.
	
% evaluate the query generating N samples
eval_query_exact(ExactVar,PosEvidence,NegEvidence,Query,N,FP) :-
	bb_put(exact,0.0),
	(
		user:distributionalclause(ExactVar,Distribution,Body,_),
		user:Body,
		ground(Distribution),
		ground(ExactVar),
		Distribution2=Distribution,
		(
			Distribution=uniform(D) ->
			(
				length(D,Size),
				W is 1/Size,
				member(Val,D),
				(
					get_magic(backward) ->
						eval_query_backward([ExactVar~=Val|PosEvidence],NegEvidence,Query,N,P,Succ_Sum,Sum)
					;
						eval_query_forward([ExactVar~=Val|PosEvidence],NegEvidence,Query,N,P,Succ_Sum,Sum)
				),
				bb_get(exact,FinalP),
				FinalP2 is FinalP+P*W,
				write(ExactVar),write(' = '),write(Val),write(' W '),write(W),write(' P='),write(P),nl,
				bb_put(exact,FinalP2),
				fail;
				true
			)
			;
			(
				Distribution2=finite(D2),
				member(W:Val,D2),
				(
					get_magic(backward) ->
						eval_query_backward([ExactVar~=Val|PosEvidence],NegEvidence,Query,N,P,Succ_Sum,Sum)
					;
						eval_query_forward([ExactVar~=Val|PosEvidence],NegEvidence,Query,N,P,Succ_Sum,Sum)
				),
				bb_get(exact,FinalP),
				FinalP2 is FinalP+P*W,
				bb_put(exact,FinalP2),
				fail;
				true
			)
		)
	),
	bb_delete(exact,FP),
	!.


	
% evaluate the query generating N samples using forward reasoning
eval_query_forward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		clean_sample(sampled),
		init_query(sampled,Query),
		init_query_list(sampled,PosEvidence),
		init_query_list(sampled,NegEvidence),
		generate_sample_pr(sampled,NewWeight,MaxP),
		check_evidence(sampled,PosEvidence,NegEvidence),
		% write(NewWeight),nl,
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + NewWeight,
		bb_put(sample_sum,New_Sum),
		(
			query_proof(sampled,Query)
			->
			(
				bb_get(succeeding_sample_sum,Old),
				New is Old+NewWeight,
				bb_put(succeeding_sample_sum,New)
			);
		  	true
		),
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		),
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

% evaluate the query generating N samples with backward reasoning NOT COMPLETE
eval_query_backward(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		check_evidence_backward(sampled,W1),
		check_evidence(sampled,PosEvidence,NegEvidence),
		W1>0,
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward(sampled,Query)%proof_query_backward_eval(sampled,sampled,Query,W2)
			->
			(
				W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		),
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

% to check
eval_query_backward_eval(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		clean_sample(sampled),
		clean_sample(sampledtemp),
		abolish_all_tables,
		check_evidence_backward(sampled,W1),
		check_evidence(sampled,PosEvidence,NegEvidence),
		W1>0,
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward_eval(sampled,sampledtemp,Query,W2)
			->
			(
				%W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		),
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.
% evaluate the query generating N samples with backward reasoning NOT COMPLETE
eval_query_backward(ImpossibleEvidence,PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		assert_impossible_evidence(sampled,ImpossibleEvidence),
		check_evidence_backward(sampled,W1),
		check_evidence(sampled,PosEvidence,NegEvidence),
		W1>0,
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward(sampled,Query)%proof_query_backward_eval(sampled,sampled,Query,W2)
			->
			(
				W2=1.0,
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		(
		get_debug(true)->
			write('Particle '),write(I),write(' w '),writeln(W1),nl,findall(A,(recorded(sampled,A,_), write(A),nl),_),nl,writeln('------------------------------------');
			true
		),
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.
% New inference
% it works if there is one proof for each variable
eval_query_backward_exp(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	(
		between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		check_evidence_backward_exp(sampled,[],Newvars,W1),
		check_evidence_exp(sampled,Newvars,PosEvidence,NegEvidence),
		W1>0,
		%write('Sampled E '),writeln(Newvars),
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward_exp_eval(sampled,Newvars,Newvars2,Query,W2)
			->
			(
				%write('Sampled Q '),writeln(Newvars2),
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
				bb_get(succeeding_sample_sum,Old),
				New is Old+W1*W2,
				%writeln(New),
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		ps,
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

eval_query_backward_exact(PosEvidence,NegEvidence,Query,N,P,Succ_Sum,Sum) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(succeeding_proofs,0),
	(
		%between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		
		check_evidence_backward_exact(sampled,[],Newvars,W1),
		check_evidence_exp(sampled,Newvars,PosEvidence,NegEvidence),
		W1>0,
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		
		
		%write(W1),write(' Evidence Sampled '),writeln(Newvars),
		
		proof_query_backward_exact_eval(sampled,Newvars,Newvars2,Query,W2),
		bb_get(succeeding_sample_sum,Old),
		New is Old+W1*W2,
		%writeln(New is Old+W1*W2),
		bb_put(succeeding_sample_sum,New),	
		
		bb_get(succeeding_proofs,OldC),
		C is OldC+1,
		bb_put(succeeding_proofs,C),
		
		append([Newvars,Newvars2],Un),
		TempP is New/New_Sum,
		write(TempP),write(' Sampled '),writeln(Un),
		
		(C<N ->
			fail;
			true
		);

		true
	),
	%clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(succeeding_sample_sum,Succ_Sum),
	P is Succ_Sum/Sum,
	retractall(user:evidence(_,_)),
	!.

eval_query_backward_exact_distrib(PosEvidence,NegEvidence,X,Query,N,Distr) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,[]),
	bb_put(succeeding_proofs,0),
	(
		%between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		
		check_evidence_backward_exact(sampled,[],Newvars,W1),
		check_evidence_exp(sampled,Newvars,PosEvidence,NegEvidence),
		W1>0,
		
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		
		
		%write(W1),write(' Evidence Sampled '),writeln(Newvars),
		
		proof_query_backward_exact_eval(sampled,Newvars,Newvars2,Query,W2),
		bb_get(succeeding_sample_sum,OldD),
		
		sum_distrib(finite(OldD),finite([W1:X]),W2,finite(NewD)),
		%write(NewD),nl,
		bb_put(succeeding_sample_sum,NewD),
		
		bb_get(succeeding_proofs,OldC),
		C is OldC+1,
		bb_put(succeeding_proofs,C),
		
		append([Newvars,Newvars2],Un),
		%write(' Sampled '),writeln(Un),
		
		(C<N ->
			fail;
			true
		);

		true
	),

	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		Distr=[]
		;
		divideby(SUCC,TOT,Distr)
	),
	retractall(user:evidence(_,_)),
	!.
	
eval_query_backward_distrib(PosEvidence,NegEvidence,X,Query,N,Distr) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,[]),
	(
		between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		check_evidence_backward(sampled,W1),
		check_evidence(sampled,PosEvidence,NegEvidence),
		W1>0,
		%write('Sampled E '),writeln(Newvars),
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward(sampled,Query)
			->
			(
				%write('Sampled Q '),writeln(Newvars2),
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
								
				bb_get(succeeding_sample_sum,Old),
				sum_distrib(finite(Old),finite([W1:X]),1.0,finite(New)),
				%write((List,Weight,New)),nl,
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		%ps,
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		Distr=[]
		;
		divideby(SUCC,TOT,Distr)
	),
	retractall(user:evidence(_,_)),
	!.
	
% NOT COMPLETE
eval_query_backward_exp_distrib(PosEvidence,NegEvidence,X,Query,N,Distr) :-
	%magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(succeeding_sample_sum,[]),
	(
		between(1,N,I),
		clean_sample(sampled),
		abolish_all_tables,
		check_evidence_backward_exp(sampled,[],Newvars,W1),
		check_evidence_exp(sampled,Newvars,PosEvidence,NegEvidence),
		W1>0,
		%write('Sampled E '),writeln(Newvars),
		bb_get(sample_sum,Old_Sum),
		New_Sum is Old_Sum + W1,
		bb_put(sample_sum,New_Sum),
		(
			proof_query_backward_exp_eval(sampled,Newvars,Newvars2,Query,W2)
			->
			(
				%write('Sampled Q '),writeln(Newvars2),
				%write('ok w: '),
				%check_evidence(sampled,PosEvidence,NegEvidence),
				%write(Query),nl,
								
				bb_get(succeeding_sample_sum,Old),
				Weight is W1*W2,
				sum_distrib(finite(Old),finite([Weight:X]),1.0,finite(New)),
				%write((List,Weight,New)),nl,
				bb_put(succeeding_sample_sum,New)
			)
			;
		  	true
		),		  	
		%ps,
		
		fail;

		true
	),
	%clean_sample(sampled),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		Distr=[]
		;
		divideby(SUCC,TOT,Distr)
	),
	retractall(user:evidence(_,_)),
	!.
	


prova :-
	(
		user:hardclause(Head,Body,Pr),
		write(Head),write(' '),write(Body),nl,
		(
			Head=callmagic(_)
			->
				fail;
				true
		)
	).

get_counts(L,Sum) :-
	recorded(ris,count(Value,Count),K),
	erase(K),
	P is Count/Sum,
	L=[(Value,P)|H],
	get_counts(H,Sum).

get_counts([],Sum) :-
	\+recorded(ris,count(Value,Count),K),
	!.
	
print_hardclause :-
	user:hardclause(_H,_B,_Pr),
	numbervars(user:hardclause(_H,_B,_Pr),1,_),
	write(_H),
	write(' :- '),
	write(_B),
	write(' Pr '),
	write(_Pr),
	nl,fail;true.

print_distributionalclause :-
	user:distributionalclause(_H,_D,_B,_Pr),
	numbervars(user:distributionalclause(_H,_D,_B,_Pr),1,_),
	write(_H),
	write(' ~ '),
	write(_D),
	write(' :- '),
	write(_B),
	write(' Pr '),
	write(_Pr),
	nl,fail;true.

print_distributionalclause_global :-
	recorded(global,distributionalclause(_H,_D,_B,_Pr),_),
	numbervars(user:distributionalclause(_H,_D,_B,_Pr),1,_),
	write(_H),
	write(' ~ '),
	write(_D),
	write(' :- '),
	write(_B),
	write(' Pr '),
	write(_Pr),
	nl,fail;true.

print_all :-
	print_hardclause,
	print_distributionalclause.
	
init :-
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	clean_sample(sampled),
	magic,
	write('init'),nl.

	
/*

% OLD! evaluate the distribution TO FINISH!!
eval_distribution(PosEvidence,NegEvidence,Distr,N,List_Distr,Sum,SumTot) :-
	magic,
	get_max_priority(MaxP),
	retractall(user:evidence(_,_)),
	assert_evidence(PosEvidence,1),
	assert_evidence(NegEvidence,0),
	bb_put(sample_sum,0.0),
	bb_put(sample_sum_tot,0.0),
%	bb_put(succeeding_sample_sum(Value),0.0),
	(
		between(1,N,_),
		clean_sample(sampled),
		init_query(sampled,Distr ~= Value),
		init_query_list(sampled,PosEvidence),
		init_query_list(sampled,NegEvidence),
		generate_sample_pr(sampled,Weight,MaxP),
		check_evidence(sampled,PosEvidence,NegEvidence),
		bb_get(sample_sum_tot,Old_SumT),
		New_SumT is Old_SumT + Weight,
		bb_put(sample_sum_tot,New_SumT),
		(
			recorded(sampled,Distr ~= Value,_)
			->
			(
				bb_get(sample_sum,Old_Sum),
				New_Sum is Old_Sum + Weight,
				bb_put(sample_sum,New_Sum),
				recorded(ris,count(Value,Old),Key)
				->
				(
					erase(Key),
					New is Old+Weight,
					recorda(ris,count(Value,New),_)
				);
				(
					recorda(ris,count(Value,Weight),_)
				)			
			);
		  true
		),
		fail;

		true
	),
	clean_sample(sampled),
	bb_delete(sample_sum,Sum),
	bb_delete(sample_sum_tot,SumTot),
	get_counts(List_Distr,SumTot),
	retractall(user:evidence(_,_)),
	!.
	
proof_query_backward(Key,findall_forward(X,Y,Z)) :-
	bb_put(lista,[]),
	(
		%copy_term(X,Term),
		proof_query_backward(Key,Y),
		bb_get(lista,L),
		NewL=[X|L],
		bb_put(lista,NewL),
		fail;
		true
	),
	bb_delete(lista,Z),
	!.
*/
