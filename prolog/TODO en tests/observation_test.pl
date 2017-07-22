
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

o(1):0  ~ uniform([cup,soda]).
o(1):t+1 ~ val(Type) :=
	o(1):t ~= Type.

observation(type):t+1 ~ finite([0.9:cup,0.1:soda]) :=
	o(1):t+1 ~= cup.

observation(type):t+1 ~ finite([0.9:soda,0.1:cup]) :=
	o(1):t+1 ~= soda.

start :-
	init_particle(1000),
	eval_query_particle2(O2,current(o(1))~=O2,1000,D2),
	writeln(D2),
	step_particle([action(null)],[observation(type)~=cup],1000),
	eval_query_particle2(O22,current(o(1))~=O22,1000,D22),
	writeln(D22).

