
:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- use_module('knowledge_processing.pl').

:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module('knowledge_base.pl').
:- use_module('command_processing.pl').
:- use_module('perception_module.pl').
:- set_magic(backward). 

builtin(findall(_,_,_)).
builtin(A=B).

satisfiable(A=B).
satisfiable(A\==B).
satisfiable(A\=B).
satisfiable(A > B).
satisfiable(A < B).
satisfiable(A >= B).
satisfiable(A =< B).
satisfiable(findall(_,_,_)).

checkvalue(true,1.0).
checkvalue(false,0.0).

builtin(\+A) :-
	builtin(A).
builtin(write(_)).

lifted(true).
raoblackwellisation(false).

watest :-
	
	%nobjs:0 ~ val(X) := true,
	writeln(X),
	init_particle(10),
	check_particles,
	eval_query_particle2(WA1,current(wa)~=WA1,10,D1),
	writeln('Wanted action:'), writeln(D1),
	
	step_particle([],[o(2)~=soda],10),
	check_particles,
	
	eval_query_particle2(WA2,current(wa)~=WA2,10,D2),
	writeln('Wanted action:'), writeln(D2).


check_particles :-
	(
		between(1,10,I),
		writeln(' '),
		write('Particle: '), writeln(I),
		printp(I),
		fail;
		true
	).


o(1):0  ~ uniform([cup,soda]).
o(2):0  ~ uniform([cup,soda]).

o(N):t+1 ~ val(Type) :=
	o(N):t ~= Type.

observation(type(X)):t+1 ~ val(true) :=
	o(X):t+1 ~= T2,
	T = T2.

nobjs:0 ~ val(2) := true.
nobjs:t+1 ~ val(NO) := 
	nobjs:t ~= NO.

get_objects:t ~ val(L) :=
	nobjs:t ~= NO,
	get_objects2(1,NO,L).

get_objects2(NO,NO,[NO]).
get_objects2(N,NO,[N|L]) :-
	N<NO,
	N2 is N+1,
	get_objects2(N2,NO,L).

wa:t ~ uniform(L2) :=
	get_objects:t ~= L1,
	precondition_focus_prune(L1):t ~= L2.
%wa:t+1 ~ val(WA) :=
	%wa:t ~= WA.

precondition_focus_prune([]):t ~ val([]) := true.
precondition_focus_prune([X|R]):t ~ val([X|R2]) :=
	o(X):t~=cup, 
	precondition_focus_prune(R):t ~= R2.
precondition_focus_prune([X|R]):t ~ val(R2) :=
	precondition_focus_prune(R):t ~= R2.

