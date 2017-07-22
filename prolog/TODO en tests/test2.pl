%%% -*- Mode: Prolog; -*-
:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').

lifted(false).
raoblackwellisation(false).
:- set_magic(backward).
:- set_debug(false).

test :-
	N is 10,
	init_particle(N),
	eval_query_particle2(B,current(has_category(humans_ca,human1))~=B,N,R),
	writeln(R),nl,

	step_particle([nonhuman],[],N),	
	eval_query_particle2(B1,current(has_category(humans_ca,human1))~=B1,N,R1),
	writeln(R1),nl,

	step_particle([human],[],N),	
	eval_query_particle2(B2,current(has_category(humans_ca,human1))~=B2,N,R2),
	writeln(R2),nl.

has_category(humans_ca, human1):0 ~ val(true) := true.%this is the prior knowledge that human1 is a human
has_category(Cat,H):t+1 ~ val(true) :=
	human,
	has_category(Cat,H):t ~= B.
has_category(Cat,H):t+1 ~ val(false) :=
	nonhuman,
	has_category(Cat,H):t ~= B.
has_category(Cat,H):t+1 ~ val(B) :=
	has_category(Cat,H):t ~= B.
