
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
builtin(determ(_)).

lifted(true).
raoblackwellisation(false).

determ(1).

o(1):0  ~ finite([0.4:cup,0.6:soda]).
o(1):t+1 ~ val(Type) :=
	o(1):t ~= Type.
o(2):0  ~ finite([0.5:cup,0.5:soda]).
o(2):t+1 ~ val(Type) :=
	o(2):t ~= Type.

start :-
	init_particle(10),
	eval_query_particle_exact(O,(current(o(O))~=cup,determ(O)),10,D2),
	writeln(D2).
