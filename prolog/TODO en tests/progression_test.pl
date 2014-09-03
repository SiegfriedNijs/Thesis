
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

start :-
	init_particle(20),
	check_particles(20),
	
	step_particle([action(null)],[o(1)~=soda],20),
	check_particles(20),
	step_particle([action(act)],[],20),
	check_particles(20),
	eval_query_particle2(O2,current(o(2))~=O2,20,D2),
	writeln(D2).


check_particles(N) :-
	(
		between(1,N,I),
		writeln(' '),
		write('Particle: '), writeln(I),
		printp(I),
		fail;
		true
	).


o(1):0  ~ uniform([cup,soda]).
o(1):t+1 ~ val(Type) :=
	o(1):t ~= Type.

o(2):0  ~ val(notknown) := true.
o(2):t+1 ~ uniform([a,b,c,d,e,f,g,h,i]) :=
	action(act).
o(2):t+1 ~ val(Type) :=
	%action(null),
	o(2):t ~= Type.




nobjs:0 ~ val(2) := true.
nobjs:t+1 ~ val(NO) := 
	nobjs:t ~= NO.


