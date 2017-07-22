

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

test:0 ~ uniform([1,2,3,4,5]).
test:t+1 ~ val(X) := 
	test:t ~= X.

start :-
	init_particle([test~=5],2),
	check_particles(2),
	
	step_particle([],[],2),
	check_particles(2),
	step_particle([],[],2),
	check_particles(2).


check_particles(N) :-
	(
		between(1,N,I),
		writeln(' '),
		write('Particle: '), writeln(I),
		printp(I),
		fail;
		true
	).
