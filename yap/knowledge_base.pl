

%%% -*- Mode: Prolog; -*-

:- use_module(library(lists)).
:- use_module('knowledge_processing.pl').

:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module('knowledge_base.pl').
:- use_module('command_processing.pl').
:- use_module('perception_module.pl').
:- set_magic(backward). 

lifted(false).
raoblackwellisation(false).

%:- style_check(all).

:- set_magic(backward). %TODO
:- set_debug(false).
%in DCPF you can print the content on any particle you have using printp(V) with V the particle index, a number from 1 to the number of particles.

:- dynamic instance2/2.
:- dynamic has_colour/2.

%%TODO
builtin(true).
builtin(is_subcategory(_,_)).
builtin(category(_)).
builtin(generate_content_name(_,_)).
builtin(atomic_concat(_,_)).
builtin(retract(_)).
builtin(asserta(_)).
builtin(A is B).
builtin(perform(_,_)).
builtin(succ(_,_)).
builtin(timepoint(_)).
builtin(A=B).
builtin(A\==B).
builtin(A\=B).
builtin(A > B).
builtin(A < B).
builtin(has_dimensions(_,_)).
builtin(get_action(_,_)).
builtin(get_instances(_,_,_)).
builtin(member(_,_)).
builtin(pickup(_)).
builtin(gives_command(_,_,_,_)).
builtin(myself(_)).
builtin(workingplace(_)).
builtin(distance_interval(_,_,_)).
builtin(command_content(_,_)).
builtin(get_actionname(_,_)).
builtin(subcategory(_,_)).
builtin(findall(_,_,_)).
builtin(instance2(_)).
builtin(perceived(_)).
builtin(prune_instances(_,_,_,_)).
builtin(prune_instances2(_,_)).
builtin(ask_question(_,_,_)).
builtin(implies_choice(_,_,_,_)).
builtin(find_object_actedon_clues(_,_,_)).
builtin(list_to_tuple(_,_)).
builtin(convert_to_query(_,_)).
builtin(length(_,_)).
builtin(remove_duplicates(_,_)).
builtin(segm(_,_)).
builtin(get_objects2(_,_,_)).
builtin(append(_,_,_)).
builtin(nth1(_,_,_)).
builtin(perf(_,_,_,_)).

satisfiable(findall(_,_,_)).

builtin(\+A) :-
	builtin(A).

checkvalue(true,1.0).
checkvalue(false,0.0).

%time(distributionalclause:eval_query_backward_exp([],[],has_colour(object1)~=red,1000,P1,_,_)).
%time(distributionalclause:eval_query_backward_exp([],[],(has_category(object1)~=cups_ca,has_colour(object1)~=red),1000,P1,_,_)).
%%%%%%%%%%%%%%%%%%%%
%     distributionalclause:eval_query_backward_exp([],[],(has_category(object1)~=cups_ca,has_colour(object1)~=red),1000,P,_131957,_131958)


/*
Category hierarchy
*/

category(anything_ca).

%
category(spatialobjects_ca).
subcategory(spatialobjects_ca,anything_ca).
%%
category(things_ca).
subcategory(things_ca,spatialobjects_ca).
%%%
category(moveablethings_ca).
subcategory(moveablethings_ca,things_ca).
category(fluidholders_ca).
subcategory(fluidholders_ca,moveablethings_ca).
category(cups_ca).
subcategory(cups_ca,fluidholders_ca).
category(sodacans_ca).
subcategory(sodacans_ca,fluidholders_ca).
subcategory(fluids_ca,moveablethings_ca).
category(robotarms_ca).
subcategory(robotarms_ca,moveablethings_ca).

category(graspablethings_ca).
subcategory(graspablethings_ca,things_ca).
subcategory(cups_ca, graspablethings_ca).

%%%
category(immoveablethings_ca).
subcategory(immoveablethings_ca,things_ca).
category(tables_ca).
subcategory(tables_ca,immoveablethings_ca).
%%
category(stuff_ca).
subcategory(stuff_ca,spatialobjects_ca).
category(fluids_ca).
subcategory(fluids_ca,stuff_ca).
category(water_ca).
subcategory(water_ca,fluids_ca).
category(milk_ca).
subcategory(milk_ca,fluids_ca).
category(cola_ca).
subcategory(cola_ca,fluids_ca).
%%
category(places_ca).
subcategory(places_ca,spatialobjects_ca).
%%
category(actionperformers_ca).
subcategory(actionperformers_ca,spatialobjects_ca).
category(robots_ca).
subcategory(robots_ca,actionperformers_ca).
category(pr2_ca).
subcategory(pr2_ca, robots_ca).
category(humans_ca).
subcategory(humans_ca,actionperformers_ca).

%
category(temporalobjects_ca).
subcategory(temporalobjects_ca,anything_ca).
%%
category(processes_ca).
subcategory(processes_ca,temporalobjects_ca).
%%%
category(actions_ca).
subcategory(actions_ca,processes_ca).
%%%%


category(performableactions_ca).
subcategory(performableactions_ca,actions_ca).
subcategory(pickandplace_ca,performableactions_ca).
subcategory(pickup_ca,performableactions_ca).
subcategory(answerquestionactions_ca,performableactions_ca).
subcategory(place_ca,performableactions_ca).

category(movesomethings_ca).
subcategory(movesomethings_ca,actions_ca).
subcategory(pickandplace_ca,movesomethings_ca).
subcategory(pickup_ca,movesomethings_ca).
subcategory(place_ca,movesomethings_ca).

category(physicalactions_ca).
subcategory(physicalactions_ca, actions_ca).
%%%%%
category(pickandplace_ca).
subcategory(pickandplace_ca,physicalactions_ca).
%%%%%
category(pickup_ca).
subcategory(pickup_ca,physicalactions_ca).
%%%%%

/*
category(armmovements_ca).
subcategory(armmovements_ca,physicalactions_ca).
%%%%%
category(objectgrasps_ca).
subcategory(objectgrasps_ca,physicalactions_ca).
%%%%
category(nonphysicalactions_ca).
subcategory(nonphysicalactions_ca, actions_ca).
%%%%%
category(answerquestionactions_ca).
subcategory(answerquestionactions_ca,nonphysicalactions_ca).
%%
category(events_ca).
subcategory(events_ca,temporalobjects_ca).
*/
%
category(abstractobjects_ca).
subcategory(abstractobjects_ca,anything_ca).
%%
category(representationalobjects_ca).
subcategory(representationalobjects_ca,abstractobjects_ca).
%%%
category(communicationconcepts_ca).
subcategory(communicationconcepts_ca,representationalobjects_ca).
%%%%
category(commands_ca).
subcategory(commands_ca,communicationconcepts_ca).

/*
category(clearcommands_ca).
subcategory(clearcommands_ca,commands_ca).
category(vaguecommands_ca).
subcategory(vaguecommands_ca,commands_ca).
category(explicitactioncommands_ca).
subcategory(explicitactioncommands_ca,commands_ca).
*/
/*
category(answerquestioncommands_ca).
subcategory(answerquestioncommands_ca,commands_ca).
category(performphysicalactioncommands_ca).
subcategory(performphysicalactioncommands_ca,commands_ca).
category(evidencecommands_ca).
subcategory(evidencecommands_ca,commands_ca).
category(feedbackcommands_ca).
subcategory(feedbackcommands_ca,commands_ca).
%%%%
category(questions_ca).
subcategory(questions_ca,communicationconcepts_ca).
%%%%
category(answers_ca).
subcategory(answers_ca,communicationconcepts_ca).
%%%%
*/

/*
category(commandclues_ca).
subcategory(commandclues_ca,communicationconcepts_ca).
category(actionclues_ca).
subcategory(actionclues_ca,commandclues_ca).
category(explicitactionmentions_ca).
subcategory(explicitactionmentions_ca,actionclues_ca).
category(parameterdescriptions_ca).
subcategory(parameterdescriptions_ca,actionclues_ca).
category(performerdescriptions_ca).
subcategory(performerdescriptions_ca,parameterdescriptions_ca).
category(objectactedondescriptions_ca).
subcategory(objectactedondescriptions_ca,parameterdescriptions_ca).
category(objectclues_ca).
subcategory(objectclues_ca,commandclues_ca).
category(objectcategorydefinitions_ca).
subcategory(objectcategorydefinitions_ca,objectclues_ca).
category(objectpropertydefinitions_ca).
subcategory(objectpropertydefinitions_ca,objectclues_ca).
category(relatedobjectmentions_ca).
subcategory(relatedobjectmentions_ca,objectclues_ca).
category(effectclues_ca).
subcategory(effectclues_ca,commandclues_ca).
category(spatialclues_ca).
subcategory(spatialclues_ca,effectclues_ca).
*/
%%%%
category(missinginformation_ca).
subcategory(missinginformation_ca,communicationconcepts_ca).
%%%
/*
category(properties_ca).
subcategory(properties_ca,representationalobjects_ca).
category(objectproperties_ca).
subcategory(objectproperties_ca,properties_ca).
category(colour_ca).
subcategory(colour_ca,objectproperties_ca).
category(material_ca).
subcategory(material_ca,objectproperties_ca).
category(weight_ca).
subcategory(weight_ca,objectproperties_ca).
category(dimensions_ca).
subcategory(dimensions_ca,objectproperties_ca).
category(actionparameters_ca).
subcategory(actionparameters_ca,properties_ca).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subcategory(moveablethings_ca, actionparameters_ca).
category(actionperformers_ca).
subcategory(actionperformers_ca, actionparameters_ca).
%%%
category(contexts_ca).
subcategory(contexts_ca,representationalobjects_ca).
%%
category(mathematicalobjects_ca).
subcategory(mathematicalobjects_ca,representationalobjects_ca).

essentialparameters(pickup_ca,[object_pickedup(pickup_ca, moveablethings_ca), has_performer(pickup_ca, actionperformers_ca), uses_gripper(pickup_ca, robotarms_ca)]).
acts_on_object(pickup_ca, moveablethings_ca).
pickup(pickup_ca, spatialobjects_ca).
can_perform(pr2_ca,pickup_ca).


/* 

*/

exists:0 ~ val(true) :=
	true.
exists:t+1 ~ val(true) :=
	exists:t ~= true.

nobjs:0 ~ val(0) := true.
nobjs:t+1 ~ val(NObjs) :=
	observation(nobjs) ~= NObjs.
nobjs:t+1 ~ val(NO) := 
	nobjs:t ~= NO.

object(N):t ~ val(true) := 
	nobjs:t ~= Nu, 
	between(1,Nu,N).
object(N):t+1 ~ val(B) :=
	object(N):t ~= B.


content(N):t ~ val(true) :=
	object(N):t ~= true,
	has_category(fluidholders_ca,object(N)):t ~= true.
content(N):t ~ val(false) := 
	object(N):t ~= true,
	has_category(fluidholders_ca,object(N)):t ~= false.

nhumans ~ val(1) := true.
human(N) ~ val(true) :=
	nhumans ~= NH,
	between(1,NH,N).

myself(pr2robot1).
nrobots ~ val(1) := true.
robot(N) ~ val(true) :=
	nrobots ~= NH,
	between(1,NH,N).

ncommands ~ val(1) := true.
command(N) ~ val(true) :=
	ncommands ~= NH,
	between(1,NH,N).

has_category(pr2_ca,robot(N)):0 ~ val(true) :=
	robot(N) ~= true.
has_category(Super,robot(N)):0 ~ val(true) :=
	category(Super),
	robot(N) ~= true,
	is_subcategory(Sub,Super),
	has_category(Sub,robot(N)):t ~= true.

/*
has_category(Cat,robot(N)):0 ~ finite([1:false]) :=
	category(Cat),
	robot(N) ~= true.
*/

has_category(humans_ca,human(N)):0 ~ val(true) :=
	human(N) ~= true.
has_category(Super,human(N)):0 ~ val(true) :=
	category(Super),
	human(N) ~= true,
	is_subcategory(Sub,Super),
	has_category(Sub,human(N)):t ~= true.

/*
has_category(Cat,human(N)):0 ~ finite([1:false]) :=
	category(Cat),
	human(N) ~= true.
*/

has_category(tables_ca,table):0 ~ val(true) := true.
has_category(Super,table):0 ~ val(true) :=
	category(Super),
	is_subcategory(Sub,Super),
	has_category(Sub,table):t ~= true.
has_category(Cat,table):0 ~ val(false) :=
	category(Cat).

%%TODO
/*
has_category(spatialobjects_ca,object(N)):0 ~ val(true) :=
	object(N):t ~= true.
has_category(Super,object(N)):0 ~ val(true) :=
	object(N):t ~= true,
	category(Super),
	is_subcategory(Sub,Super),
	has_category(Sub,object(N)):t ~= true.
has_category(Cat,object(N)):0 ~ finite([1:false]) :=
	category(Cat),
	object(N):t ~= true.
*/

has_category(Cat,object(N)):t+1 ~ val(true) :=
	object(N):t ~= true,
	action(segm(object(N),_)),
	obj_category(object(N)):t+1 ~= Cat,
	%writeln(Cat),
	category(Cat).
has_category(Super,object(N)):t+1 ~ val(true) :=
	object(N):t ~= true,
	action(segm(object(N),_)),
	category(Super),
	obj_category(object(N)):t+1 ~= Cat,
	is_subcategory(Cat,Super).
has_category(Cat,object(N)):t+1 ~ val(false) :=
	category(Cat),
	object(N):t ~= true,
	action(segm(object(N),_)).
has_category(Cat,object(N)):t+1 ~ val(true) :=
	object(N):t ~= true,
	action(cat_ev(object(N),Cat)),
	category(Cat).
has_category(Super,object(N)):t+1 ~ val(true) :=
	object(N):t ~= true,
	action(cat_ev(object(N),Cat)),
	category(Super),
	is_subcategory(Cat,Super).
has_category(Cat,object(N)):t+1 ~ val(false) :=
	object(N):t ~= true,
	action(cat_ev(object(N),_)), 
	category(Cat).

/*
has_category(commands_ca,command(N)):0 ~ val(true) :=
	command(N):t ~= true.
*/
/*
has_category(Cat,command(N)):0 ~ val(false) :=
	category(Cat),
	command(N) ~= true.
*/

has_category(Cat,command(N)):t ~ val(true) :=
	comm_category(command(N)):t ~= Cat.

%soda cans only contain cola
has_category(fluids_ca,content(N)):t ~ finite([1:true]) := 
	content(N):t ~= true,
	has_category(fluidholders_ca,object(N)):t ~= true.
has_category(cola_ca,content(N)):t ~ finite([1:true]) := 
	content(N):t ~= true,
	has_category(sodacans_ca,object(N)):t ~= true.
has_category(water_ca,content(N)):t ~ finite([1:true]) := 
	content(N):t ~= true,
	has_category(cups_ca,object(N)):t ~= true.
%TODO superklassen ook nog op true zetten

has_category(Cat,H):t+1 ~ val(B) :=
	has_category(Cat,H):t ~= B.


obj_category(object(N)):0 ~ val(unknown) :=
	object(N):t ~= true.

obj_category(object(N)):t+1 ~ finite(Distr) :=
	action(segm(object(N),Distr)).

obj_category(object(N)):t+1 ~ val(Val) :=
	action(cat_ev(object(N),Val)).
obj_category(object(N)):t+1 ~ val(Cat) :=
	obj_category(object(N)):t ~= Cat.

has_colour(object(N)):0 ~ uniform([red,black,blue,white,green,purple]) :=
	object(N):t ~= true.
has_colour(object(N)):t+1 ~ val(Col) :=
	action(col(object(N),Col)).
has_colour(object(N)):t+1 ~ val(Col) :=
	has_colour(object(N)):t ~= Col.

has_category(colour_ca,red,_) ~ val(true) := true.
has_category(colour_ca,green,_) ~ val(true) := true.
has_category(colour_ca,yellow,_) ~ val(true) := true.
has_category(colour_ca,blue,_) ~ val(true) := true.
has_category(colour_ca,black,_) ~ val(true) := true.
has_category(colour_ca,white,_) ~ val(true) := true.

%%%%%%%%%%has_category(moveablethings_ca,X,T) ~= false.
has_category(spatialobjects_ca,I,0) ~= false.

has_category(Cat,I,0) ~ val(true) :=
	is_subcategory(Cat,spatialobjects_ca), %TODO is_subcateg
	%has_category(spatialobjects_ca,I,0) ~= true,%veroorzaakt loop
	obj_category(I) ~= Cat.

/*
has_category(Cat,I,T) ~ val(true) :=
	subcategory(Cat,actions_ca), %TODO is_subcateg
	%has_category(spatialobjects_ca,I,0) ~= true,
	action_category(I,T) ~= Cat.
*/

has_category(actions_ca,WA,_) ~ val(true) :=
	wants_action(H,WA,_) ~= true. 

has_category(Super,I,T) ~ val(true) :=
	%writeln(test2),
	subcategory(Sub,Super),
	%writeln(Sub),
	has_category(Sub,I,T) ~= true.

has_category(Cat,I,T2) ~ val(true) :=
	%writeln(test1),
	timepoint(T),
	T2 is T+1,
	%writeln(T2),
	has_category(Cat,I,T) ~= true.

%TODO geval voor evidence van user
has_category(Cat,I,T) ~ val(false) := true.

/*
Human needs
*/

has_need(human(N)):0 ~ val(nothing) :=
	human(N) ~= true.
has_need(human(N)):t+1 ~ val(obtain_item) :=
	human(N) ~= true,
	action(gives_command(human(N),_,Comm)),
	action(command_content(Comm,Content)),
	member(pickup(O),Content).
	%inside kitchen variant
has_need(human(N)):t+1 ~ val(Need) :=
	human(N) ~= true,
	has_need(human(N)):t ~= Need.
	
performing_action(T2) ~ val(false) :=
	timepoint(T),
	T2 is T+1,
	succes_performance(_,T).
performing_action(T2) ~ val(true) :=
	timepoint(T),
	T2 is T+1,
	start_performance(_,T).
performing_action(T2) ~ val(B) :=
	timepoint(T),
	T2 is T+1,
	performing_action(T) ~= B.
performing_action(_) ~ val(false) := true.

wants_action(human(N)):0 ~ val(false) := 	
	human(N) ~= true.
wants_action(human(N)):t+1 ~ val(true) :=
	human(N) ~= true,
	action(actiondemand(human(N))).
wants_action(human(N)):t+1 ~ val(B) :=
	human(N) ~= true,
	wants_action(human(N)):t ~= B. 

wanted_action(human(N)):t ~ uniform(L) :=
	human(N) ~= true,
	action(wabegin),
	%findall(Sub,subcategory(Sub,performableactions_ca),L2),
	performable(pickup_ca):t ~= Lpickup,
	performable(place_ca):t ~= Lplace,
	append(Lplace,Lpickup,L).
wanted_action(human(N)):t+1 ~ val(B) :=
	wanted_action(human(N)):t ~= B.

performable(pickup_ca):t ~ val(L) :=
	findall_forward((pickup,object(ID),Gripper),(current(on(object(ID),table)) ~= true,availablemeans(pickup_ca):t ~= Gripper),L).

performable(place_ca):t ~ val(L) :=
	findall_forward((place,Obj,X),(current(holding_object(X)) ~= Obj, Obj \= none),L). %TODO none controle ok?

availablemeans(pickup_ca):t ~ uniform(Means) :=
	findall_forward(X,current(holding_object(X)) ~= none,Means).

comm_category(command(N)):0 ~ val(unknown) := command(N) ~= true.
comm_category(command(N)):t+1 ~ val(performphysicalactioncommands_ca) :=
	command(N) ~= true,
	action(gives_command(H,R,command(N))),
	action(command_content(command(N),Content)),
	member(pickup(_),Content).
comm_category(command(N)):t+1 ~ uniform(L) :=
	command(N) ~= true,
	action(gives_command(H,R,command(N))),
	findall(X,subcategory(X,commands_ca),L).
comm_category(command(N)):t+1 ~ val(V) :=
	command(N) ~= true,
	comm_category(command(N)):t ~= V.
/*
comm_category(command(N)):t+1 ~ val(answerquestioncommands_ca) :=
	command(N) ~= true,
	action(gives_command(H,R,I)),
	ask_question(M,Q,T),
	implies_choice(Q,_,_,Choice),
	command_content(I,Content),
	nth1(1,Content,C1),%TODO specifieker
	member(C1,Choice).
*/

handling_command:0 ~ val(none) := true.
handling_command:t+1 ~ val(command(N)) :=
	command(N) ~= true,
	action(gives_command(H,R,command(N))).
handling_command:t+1 ~ val(C) :=
	handling_command:t ~= C.
%TODO wanneer tr op none?

waiting_answer:0 ~ val(false) := true.
waiting_answer:t+1 ~ val(true) :=
	action(question(_,_,_)).
waiting_answer:t+1 ~ val(false) :=
	action(answer(_,_,_)).
waiting_answer:t+1 ~ val(B) :=
	waiting_answer:t ~= B.
%TODO give answer

answer:0 ~ val(none) := true.
answer:t+1 ~ val(FChoice) :=
	action(question(_,choice,FChoice)).
answer:t+1 ~ val(none) :=
	action(answer(_,choice,N)),
	answer:t ~= Choices,
	nth1(N,Choices,Answer). %kan complexer met objectvermelding enzo
answer:t+1 ~ val(Val) :=
	answer:t ~= Val.

%has_focus(Action,T) ~ val(none) := 
%	action_category(Action,T) ~= none.

/*
has_focus(Action,T2) ~ uniform(L3) :=
	%writeln(focus1),
	timepoint(T),
	T2 is T+1,
	gives_command(H,_,Comm,T),
	command_content(Comm,Content),
	member(pickup(O),Content),
	findall(X,(instance2(X)),L),
	%writeln(L),
	prune_instances2(L,Comm)~=L2,
	length(L2,Length2),
	Length2 > 0,
	%writeln(l2:L2),	
	action_category(Action,T) ~= Cat,
	precondition_focus_prune(Cat,L2,L3,T) ~= true,
	length(L3,Length),
	%writeln(l3:L3),
	Length > 0.
	%writeln(ok1).	
	%inside kitchen variant

%TODO wel nodig? als we movesomething gewoon gebruiken
has_focus(Action,T2) ~ finite([1:none]) :=
	%writeln(focus2),
	timepoint(T),
	T2 is T+1,
	gives_command(H,_,Comm,T),
	command_content(Comm,Content),
	member(pickup(O),Content),
	findall(X,(instance2(X)),L),
	prune_instances2(L,Comm)~=L2,
	%writeln(l2:L2),
	length(L2,Length),
	%writeln(Length),
	Length = 0.
	%writeln(ok2).
*/

instances ~ uniform(L2) :=
	findall(X,(instance2(X)),L),
	remove_duplicates(L,L2).

/*
has_focus(human(N)):t+1 ~ val(Choice) :=
	human(N) ~= true,
	action_category(Action,T) ~=Cat,
	is_subcategory(Cat,movesomethings_ca),
	ask_question(M,Q,T),
	%writeln(fff:T),
	%writeln(ddddt:Q),
	%writeln(ssst:Action),
	implies_choice(Q,focus,Action,FChoice),
	%writeln(ch:FChoice),
	gives_command(H,M,C,T),
	%writeln(ddddt:C),
	has_category(answerquestioncommands_ca,C,T2)~=true,
	command_content(comm2,Content),%TODO
	%writeln(Content),
	nth1(1,Content,Choice).
*/ %TODO

get_objects:t ~ val(L) :=
	nobjs:t ~= NO,
	get_objects2(1,NO,L).

get_objects2(NO,NO,[object(NO)]).
get_objects2(N,NO,[object(N)|L]) :-
	N<NO,
	N2 is N+1,
	get_objects2(N2,NO,L).

get_contents:t ~ val(L) :=
	nobjs:t ~= NO,
	get_contents2(1,NO):t ~= L.

get_contents2(N,NO):t ~ val([]) :=
	N>NO.
get_contents2(N,NO):t ~ val([content(N)|L]) :=
	N=<NO,
	N2 is N+1,
	content(N):t ~= true,
	get_contents2(N2,NO):t ~= L.
get_contents2(N,NO):t ~ val(L) :=
	N=<NO,
	N2 is N+1,
	content(N):t ~= false,
	get_contents2(N2,NO):t ~= L.

holding_object(left_gripper):0 ~ val(none) := true.
holding_object(left_gripper):t+1 ~ val(X) :=
	holding_object(left_gripper):t ~= X.
%TODO place actie

holding_object(right_gripper):0 ~ val(none) := true.
holding_object(right_gripper):t+1 ~ val(X) :=
	holding_object(right_gripper):t ~= X.


has_focus(human(N)):0 ~ val(none) := 
	human(N) ~= true.
has_focus(human(N)):t+1 ~ uniform(L3) :=
	human(N) ~= true,
	myself(M),
	action(gives_command(human(N),_,Comm)),
	action(command_content(Comm,Content)),
	action_category(human(N)):t+1 ~=Cat,
	get_objects:t ~= L1,
	get_contents:t ~= L11,
	append(L1,L11,L),
	%length(L,Length),
	%Length > 0,	
	precondition_focus_prune(Cat,L):t ~= L2,
	%length(L2,Length2),
	%Length2 > 0,
	prune_instances2(L2,Comm):t~=L3.
	%length(L3,Length3),
	%Length3 > 0.
has_focus(human(N)):t+1 ~ val(F) :=
	human(N) ~= true,
	action(answer(focus,choice,N2)),
	answer:t ~= L,
	nth1(N2,L,F).
has_focus(human(N)):t+1 ~ uniform([answer]) :=
	action_category(human(N)):t+1 ~= answerquestionactions_ca.
has_focus(human(N)):t+1 ~ val(F) :=
	human(N) ~= true,
	has_focus(human(N)):t ~= F.

focus_constraints(_,_):t ~ val(false) :=
	true.

performing:0 ~ val(none) := true.
performing:t+1 ~ val(Perf) :=
	action(start_perf(Perf)).
performing:t+1 ~ val(none) :=
	performing:t ~= Perf,
	action(success_perf(Perf)).
performing:t+1 ~ val(none) :=
	performing:t ~= Perf,
	action(notsuccess_perf(Perf)).
performing:t+1 ~ val(Perf) :=
	performing:t ~= Perf.

%good_perform:0 ~ val(none) := true.

%Performance approval.
good_perform(Perf):t+1 ~ uniform([true,false]) :=
	action(start_perf(Perf)).
good_perform(Perf):t+1 ~ val(true) :=
	action(feedback(Perf,good)).
good_perform(Perf):t+1 ~ val(B) :=
	good_perform(Perf):t ~=B.

precondition_focus_prune(pickup_ca,[]):t ~ val([]) := true.
precondition_focus_prune(pickup_ca,[H|R]):t ~ val([H|R2]) :=
	has_category(moveablethings_ca, H):t ~= true,
	on(H,table):t ~= true, 
	precondition_focus_prune(pickup_ca,R):t ~= R2.
precondition_focus_prune(pickup_ca,[content(N)|R]):t ~ val([content(N)|R2]) :=
	content(N):t ~= true,
	has_category(fluids_ca, content(N)):t ~= true,
	on(object(N),table):t ~= true, %TODO algemene workspace
	%TODO graspable?
	precondition_focus_prune(pickup_ca,R):t ~= R2.
precondition_focus_prune(pickup_ca,[H|R]):t ~ val(R2) :=
	%TODO ok zo?
	precondition_focus_prune(pickup_ca,R):t ~= R2.
precondition_focus_prune(place_ca,[]):t ~ val([]) := true.
precondition_focus_prune(place_ca,[H|R]):t ~ val([H|R2]) :=
	has_category(moveablethings_ca, H):t ~= true,
	on(H,table):t ~= false, 
	precondition_focus_prune(place_ca,R):t ~= R2.
precondition_focus_prune(place_ca,[H|R]):t ~ val(R2) :=
	precondition_focus_prune(place_ca,R):t ~= R2.


has_start(Action,T) ~ val((Start)) :=
	%writeln(start2),
	action_category(Action,T) ~= pickup_ca,
	has_focus(Action,T) ~= Object,
	has_position(Object,T) ~= Start.
has_start(Action,T) ~ val((Start)) :=
	%writeln(start2),
	action_category(Action,T) ~= pickup_ca,
	has_focus(Action,T) ~= Content,
	holds_content(O,Content) ~= true,
	has_position(O,T) ~= Start.
has_start(Action,T) ~ val((Start)) :=
	%writeln(start3),
	action_category(Action,T) ~= answerquestionactions_ca,
	myself(Start).
has_start(Action,T) ~ val((none)) :=
	%writeln(start1),
	has_focus(Action,T) ~= X.%TODO TODO

prune_instances([],_,_,[]) ~ val(true) := true.
prune_instances([H|R],Cat,T,[H|R2]) ~ val(Val) :=
	has_category(Cat,H,T) ~=true,
	prune_instances(R,Cat,T,R2) ~= Val.
prune_instances([H|R],Cat,T,R2) ~ val(Val) :=
	has_category(Cat,H,T) ~=false,
	prune_instances(R,Cat,T,R2) ~= Val.


prune_instances2([],_):t ~ val([]) := true.
prune_instances2([H|R],Comm):t ~ val([H|R2]) :=
	focus_constraints(H,Comm):t ~= true,
	prune_instances2(R,Comm):t ~= R2.
prune_instances2([H|R],Comm):t ~ val(R2) :=
	focus_constraints(H,Comm):t ~= false,
	prune_instances2(R,Comm):t ~= R2.


has_performer(A,T) ~ val(M) :=
	wants_action(H,A,T) ~= true, %phttps://www.facebook.com/lus wants action from me
	has_category(performableactions_ca,A,T) ~= true,
	myself(M).
has_performer(A,T) ~ uniform(L) :=
	has_category(actions_ca,A,T) ~= true,
	findall(X,has_category(actionperformers_ca,X,T),L).

has_start(A,T) ~ val(onsurface) := %TODO beter
	wants_action(H,A,T) ~= true, 
	has_category(movesomethings_ca,A,T) ~= true,
	%in kitchen
	workingplace(Surface).
%todo

has_performer(pickup_ca, actionperformers_ca).

/*
Subcategory transitivity.
*/
is_subcategory(Category1,Category2) :-
	findall(X,subcategory(X,Category2),List),
	%writeln(List),
	length(List,Length),
	Length =\= 0,
	is_subcategory2(Category1,List).
is_subcategory(Category1,Category2) :-
	subcategory(Category1,Category2).
is_subcategory2(Category1,[H|R]) :-
	is_subcategory(Category1,H).
is_subcategory2(Category1,[H|R]) :-
	is_subcategory2(Category1,R).

%TODO get subcategories die recursief de lijst maakt
%TODO TODO
has_category(spatialobjects_ca).
holding_object(robotarms_ca, moveablethings_ca, time_ca).
%%%%%%%%%%%has_category(anything_ca).


/*
Defines when an object is near another one.
*/
near(O,O):t ~ finite([1:true]) := true.
near(O1,O2):t ~ finite([1:true]) :=
	has_position(O1):t ~= (X1,Y1,Z1),
	has_position(O2):t ~= (X2,Y2,Z2),
	has_dimensions(O1):t ~= (Xrange1,Yrange1,Zrange1),
	has_dimensions(O2):t ~= (Xrange2,Yrange2,Zrange2),

	X1min is X1 - (Xrange1/2),
	X1max is X1 + (Xrange1/2),
	X2min is X2 - (Xrange2/2),
	X2max is X2 + (Xrange2/2),
	distance_interval([X1min,X1max],[X2min,X2max],DistX),
	DistX < 10,

	Y1min is Y1 - (Yrange1/2),
	Y1max is Y1 + (Yrange1/2),
	Y2min is Y2 - (Yrange2/2),
	Y2max is Y2 + (Yrange2/2),
	distance_interval([Y1min,Y1max],[Y2min,Y2max],DistY),
	DistY < 10,

	Z1min is Z1 - (Zrange1/2),
	Z1max is Z1 + (Zrange1/2),
	Z2min is Z2 - (Zrange2/2),
	Z2max is Z2 + (Zrange2/2),
	distance_interval([Z1min,Z1max],[Z2min,Z2max],DistZ),
	DistZ < 10.
near(O1,O2):t ~ val(false) := true.


/*
Definition of an object being on another one at time T.
*/
on(O,O):t ~ finite([1:false]) := true.
on(O1,O2):t ~ finite([1:true]) :=
		has_position(O1):t ~= (X1,Y1,Z1),
		has_position(O2):t ~= (X2,Y2,Z2),
		has_dimensions(O1):t ~= (Xrange1,Yrange1,Zrange1),
		has_dimensions(O2):t ~= (Xrange2,Yrange2,Zrange2),
	
		Xlow is X2-(Xrange2/2),
		Xhigh is X2 + (Xrange2/2),
		X1 > Xlow,
		X1 < Xhigh,
		Ylow is Y2-(Yrange2/2),
		Yhigh is Y2 + (Yrange2/2),
		Y1 > Ylow,
		Y1 < Yhigh,
		ZZ is integer(Z2 + (Zrange1/2) + (Zrange2/2)),
		Zdiff is abs(Z1-ZZ),
		Zdiff < 2. %TODO 
on(O1,O2):t ~ finite([1:false]) := true.

/*
Definition of an object being left of another one at time T.
*/
%TODO definition taking into account person location
leftof(O,O):t ~ val(false) := true.
leftof(O1,O2):t ~ finite([1:true]) :=
	has_position(O1):t ~= (X1,Y1,Z1),
	has_position(O2):t ~= (X2,Y2,Z2),
	has_dimensions(O1):t ~= (Xrange1,Yrange1,Zrange1),
	has_dimensions(O2):t ~= (Xrange2,Yrange2,Zrange2),

	X1max is X1 + (Xrange1/2),
	X2min is X2 - (Xrange2/2),
	X1max < X2min. 
leftof(O1,O2):t ~ finite([1:false]) := true.

/*
Definition of an object being right of another one at time T.
*/
rightof(O,O):t ~ val(false) := true.
rightof(O1,O2):t ~ finite([1:true]) :=
	has_position(O1):t ~= (X1,Y1,Z1),
	has_position(O2):t ~= (X2,Y2,Z2),
	has_dimensions(O1):t ~= (Xrange1,Yrange1,Zrange1),
	has_dimensions(O2):t ~= (Xrange2,Yrange2,Zrange2),
	X1min is X1 - (Xrange1/2),
	X2max is X2 + (Xrange2/2),
	X1min > X2max. 
rightof(O1,O2):t ~ finite([1:false]) := true.
%TODO frontof,backof,between

%cups hold water, cola or milk
has_category(Content) ~ finite([0.4:water_ca,0.3:cola_ca:0.3:milk_ca]) := 
	holds_content(Container,Content) ~= true,
	has_category(Container) ~= cups_ca.
%red cups only hold cola
has_category(Content) ~ finite([1:cola_ca]) := 
	holds_content(Container,Content) ~= true,
	has_category(Container) ~= cups_ca,
	has_colour(Container) ~= red.
%blue cups only hold water
has_category(Content) ~ finite([1:water_ca]) := 
	holds_content(Container,Content) ~= true,
	has_category(Container) ~= cups_ca,
	has_colour(Container) ~= blue.
%white cups only hold milk
has_category(Content) ~ finite([1:milk_ca]) := 
	holds_content(Container,Content) ~= true,
	has_category(Container) ~= cups_ca,
	has_colour(Container) ~= white.	
	
%cup is red, blue or white
has_colour(Cup) ~ finite([0.8:red,0.1:blue,0.1:white]) := 
	has_category(Cup)~= cups_ca.
%table brown or black
has_colour(Table) ~ finite([0.8:brown,0.2:black]) := 
	has_category(Table)~= tables_ca.

%zero pint is at left side of table on the ground




/*
Human preference models.
*/
commandcat(human1) ~ finite([0.9:performphysicalactioncommands_ca,0.1:answerquestioncommands_ca]) := 	true.
commandcat(human2) ~ finite([0.2:performphysicalactioncommands_ca,0.8:answerquestioncommands_ca]) := 	true.
commandcat(unknownhuman) ~ finite([0.5:performphysicalactioncommands_ca,0.5:answerquestioncommands_ca]) := true.

objectaction(human1) ~ finite([0.6:pickup_ca,0.4:pickandplace_ca]) := true.
objectaction(human2) ~ finite([0.1:pickup_ca,0.9:pickandplace_ca]) := true.
objectaction(unknownhuman) ~ finite([0.5:pickup_ca,0.5:pickandplace_ca]) := true.

/*
Action models.
*/
wanted_effects(pickup_ca, [holding_object(_,_,_)]).
has_effect(A,O,T) ~ finite([0.8: [holding_object(Arm,O,T)~=true,on(O,table1)~=false], 0.2: nothing]) :=	
	actioncategory(A) ~= pickup_ca,
	on(O,table1,T)~=true.
	%algemene tafel/surface insteken

has_effect(A,O,T) ~ finite([0.7: movedsomething(O,T), 0.3: nothing]) :=			
	actioncategory(A) ~= pickandplace_ca.

actioncategory(A) ~ finite([1:pickup_ca]) :=
	timepoint(T),
	T2 is T+1,
	holding_object(Arm,O,T2) ~=true,
	holding_object(Arm,O,T) ~=false,
	perform(A,T).
	
object_pickedup(A)~ finite([1:O]) :=
	timepoint(T),
	T2 is T+1,
	holding_object(Arm,O,T2) ~=true,
	holding_object(Arm,O,T) ~=false,
	perform(A,T).
	
uses_gripper(A)~ finite([1:Arm]) :=
	timepoint(T),
	T2 is T+1,
	holding_object(Arm,O,T2) ~=true,
	holding_object(Arm,O,T) ~=false,
	perform(A,T).
%actioncategory(A) ~ finite([0.5:pickup_ca,0.5:pickandplace_ca]) := true.

has_category(pr2robot1) ~ finite([1:pr2_ca])    := true.
has_performer(A) ~ finite([0.99:pr2robot1,0.01:human1]) := has_category(A)~= pickup_ca.

perform(A,T) :- false.

has_goallocation(A) ~ finite([1:(X,Y,Z)]) := 
	movedsomething(O,T) ~= true,
	T2 is T+1,
	has_position(O,T2)~=(X,Y,Z).

has_goallocation(A) ~ finite([1:(X1,Y1,Z1)]) := 
		has_category(A)~=pickandplace_ca,
		acts_on_object(A)~=O,
		perform(A,T),
		T2 is T+1,
		has_position(O,T2) ~= (X1,Y1,Z1). 

pickup(A,Content) ~ finite([1:true]) := 
		holds_content(Container,Content) ~= true, 
		generate_content_name(Container,Content),			
		has_category(Container) ~= Cat, 
		is_subcategory(Cat,fluidholders_ca),
		pickup(A,Container) ~= true. 

pickup(A,O)~ finite([1:true]) := 
		acts_on_object(A)~=O.

/*
Object properties.
*/
has_object_property(O) ~ val(C) := has_colour(O) ~= C.
has_object_property(O) ~ val(M) := has_material(O) ~= M.
has_object_property(O) ~ val(W) := has_weight(O) ~= W.
has_object_property(O) ~ val(D) := has_dimensions(O) ~= D.

object_pick_up(A) ~ finite([1:O]) :=
	has_category(O)~= Cat,
	is_subcategory(Cat,graspablethings_ca).
	

/*
Position of object at a time, includes time progression rules.
*/
/*
has_position(O, T2) ~ finite([1:(X,Y,Z2)]) :=
	T2 > 0,
	T1 is T2-1,
	get_action(T1,A), %TODO bij alle zetten
	actioncategory(A)~= pickup_ca,
	perform(A,T1),	
	acts_on_object(A)~=O,  
	has_effect(A,O,T1) ~= pickup(_,_), %TODO bij allen
	has_position(O,T1) ~= (X,Y,Z),	
	Z2 is Z+30.			
has_position(C, T2) ~ finite([1:(X,Y,Z2)]) := 
	%writeln(pos4),			
	T2 > 0,
	T1 is T2-1,
	get_action(T1,A),
	actioncategory(A)~=pickup_ca,
	perform(A,T1),			
	acts_on_object(A)~=O, 
	holds_content(O,C)~=true,	
	has_effect(A,O,T1) ~= pickup(_,_),
	has_position(O,T1) ~= (X,Y,Z),
	Z2 is Z+30.
has_position(O, T2) ~ finite([1:(X2,Y2,Z2)]) := 
	T2 > 0,
	T1 is T2-1,
	get_action(T1,A),
	actioncategory(A)~=pickandplace_ca,
	perform(A,T1),		
	acts_on_object(A)~=O, 	
	has_effect(A,O,T1) ~= nothing,
	has_position(O,T1) ~= (X,Y,Z),
	has_goallocation(A)~= (X2,Y2,Z2). %%%
has_position(C, T2) ~ finite([1:(X2,Y2,Z2)]) :=
	%writeln(pos5),		
	T2 > 0,
	T1 is T2-1,
	get_action(T1,A),
	actioncategory(A)~=pickandplace_ca,
	perform(A,T1),		
	acts_on_object(A)~=O, 
	holds_content(O,C)~=true,	
	has_effect(A,O,T1) ~= movedsomething(_,_),
	has_goallocation(A)~= (X2,Y2,Z2). %%%%%%
has_position(C, T2) ~ finite([1:(X,Y,Z)]) := 
	%writeln(pos6),	
	T2 > 0,
	T1 is T2-1,
	get_action(T1,A),
	perform(A,T1),		
	acts_on_object(A)~=O,
	holds_content(O,C)~=true,
	has_effect(A,O,T1) ~= nothing,
	has_position(O,T1) ~= (X,Y,Z).
has_position(C, T) ~ finite([1:(X,Y,Z)]) := 
	%writeln(pos7),		
	holds_content(O,C)~=true,
	has_position(O,T) ~= (X,Y,Z).	

*/


has_position(object(N)):0 ~ finite([1:(0,0,0)]) := 
	object(N):t ~= true.	
has_position(object(N)):t+1 ~ finite([1:(X,Y,Z)]) :=
	action(pos(object(N),[X,Y,Z])).	
has_position(object(N)):t+1 ~ finite([1:(X,Y,Z2)]) :=
	action(success_perf(PName)),
	perf(PName,pickup_ca,object(N),[ExtraParams]),
	has_position(object(N)):t ~= (X,Y,Z),
	Z2 is Z+30. %TODO
	
has_position(object(N)):t+1 ~ finite([1:(X,Y,Z)]) :=
	has_position(object(N)):t ~= (X,Y,Z).

has_position(table):0 ~ finite([1:(0,0,0)]) := true.	
has_position(table):t+1 ~ finite([1:(X,Y,Z)]) :=
	action(pos(table,[X,Y,Z])).	
has_position(table):t+1 ~ finite([1:(X,Y,Z)]) :=
	has_position(table):t ~= (X,Y,Z).

has_dimensions(object(N)):0 ~ finite([1:(0,0,0)]) :=
	object(N):t ~= true.
has_dimensions(object(N)):t+1 ~ finite([1:(X,Y,Z)]) :=
	action(dim(object(N),[X,Y,Z])).
has_dimensions(object(N)):t+1 ~ finite([1:(X,Y,Z)]) :=
	has_dimensions(object(N)):t ~= (X,Y,Z).

has_dimensions(table):0 ~ finite([1:(0,0,0)]) := true.
has_dimensions(table):t+1 ~ finite([1:(X,Y,Z)]) :=
	action(dim(table,[X,Y,Z])).
has_dimensions(table):t+1 ~ finite([1:(X,Y,Z)]) :=
	has_dimensions(table):t ~= (X,Y,Z).


/*
Sample position on table.
*/
tablepos(Object) ~ gaussian([50,25],[10,0,0,5]) := true.

/*
Sample position on table near an object at a given time.
*/
tableposnearobjontable(Table,Object,T) ~ finite([1:(Xres,Yres,Zres)]) :=
	on(Object,Table,T) ~=true,
	has_position(Object,T) ~= (X,Y,Z),
	has_position(Table,T) ~= (Xt,Yt,Zt),
	has_dimensions(Object,[Xr,Yr,Zr]),
	has_dimensions(Table,[Xrt,Yrt,Zrt]),
	sample_uniform(0,360,Angle),
	Rads = Angle*(pi/180),		
	Cos is cos(Rads),
	Sin is sin(Rads), 		
	X1 is X + Cos,
	Y1 is Y + Sin,
	Xres is X + (X1-X)*Xr, %TODO max van dimensies in xy vlak obj
	Xint is integer(Xres),
	writeln(Xres),
	writeln(Xint),	
	%%between(0,100,Xint), %%	
	Yres is Y + (Y1-Y)*Xr,
	Yint is integer(Yres),
	%%between(0,50,Yint), %%
	Zres is Z.

get_current_timestep(T) :-
	timepoint(T).

distance((X1,Y1,Z1),(X2,Y2,Z2)) ~ uniform([Dist]) :=
	Dist is sqrt(exp(abs(X1-X2),2) + exp(abs(Y1-Y2),2) + exp(abs(Z1-Z2),2)).
	%writeln(test: Dist).

/*
Robot self-knowledge.
*/
possible_actions(pr2_ca,[pickup_ca,pickandplace_ca]).

/*
Container content.
*/
holds_content(object1,object1content) ~ finite([1:true]) := true.
holds_content(object2,object2content) ~ finite([1:true]) := true.
holds_content(object3,object3content) ~ finite([1:true]) := true.
generate_content_name(O,C) :-
	atomic_concat([O,content],C).

/*
Relation inheritance.
*/
has_category(Content) ~ finite([1:ContCat]) := 
				holds_content(Cup,Content)~=true,
				has_category(Cup) ~= cups_ca,
				cupcontentcat(Cup) ~= ContCat.

/*
True if object O has been moved from time T to T+1.
*/
movedsomething(O,T) ~ finite([1:true]) :=
	holds_content(O,C)~=true,
	has_category(O2)~=Cat2,
	is_subcategory(Cat2,moveablethings_ca),
	C\==O2,
	near(C,O2,T)~=false,
	T2 is T+1,	
	near(C,O2,T2)~=true.
	%writeln(ttt).
movedsomething(O,T) ~ finite([1:true]) :=
	holds_content(O,C)~=true,
	has_category(O2)~=Cat2,
	is_subcategory(Cat2,moveablethings_ca),
	C\==O2,	
	near(C,O2,T)~=true,
	T2 is T+1,	
	near(C,O2,T2)~=false.
	%writeln(sss).
movedsomething(O,T) ~ finite([1:true]) :=
	has_category(O)~=Cat,
	is_subcategory(Cat,moveablethings_ca),
	O\==O2,
	has_category(O2)~=Cat2,
	is_subcategory(Cat2,moveablethings_ca),
	%writeln(O2),		
	%writeln(moveds1: O),
	near(O,O2,T)~=false,
	%writeln(O2),
	%writeln(moveds11: O),
	T2 is T+1,	
	near(O,O2,T2)~=true.
	%writeln(moveds111: O).
movedsomething(O,T) ~ finite([1:true]) :=
	has_category(O)~=Cat,
	is_subcategory(Cat,moveablethings_ca),
	O\==O2,	
	has_category(O2)~=Cat2,
	is_subcategory(Cat2,moveablethings_ca),	
	%writeln(moveds1a: O),
	near(O,O2,T)~=true,
	%writeln(moveds11a: O),
	T2 is T+1,	
	near(O,O2,T2)~=false.
	%writeln(moveds111a: O).
movedsomething(O,T) ~ finite([1:true]) :=
	has_category(O)~=Cat,
	is_subcategory(Cat,moveablethings_ca),		
	%writeln(moveds2: O),
	has_position(O,T)~=(X1,Y1,Z1),
	%writeln(moveds22: O),
	T2 is T+1,
	has_position(O,T2)~=(X2,Y2,Z2),
	%writeln(moveds222: O),
	distance((X1,Y1,Z1),(X2,Y2,Z2))~=D,
	D >0.
	%writeln(moveds2222: O).
movedsomething(C,T)  ~ finite([1:true]) :=
	holds_content(O,C)~=true,
	movedsomething(O,T)~=true.
movedsomething(O,T) ~ finite([1:false]) := true.


add_evidence(E) :-
	asserta(E).

/*
Add new evidence to knowledge base.
*/
add_new_evidence([]).
add_new_evidence([H|R]) :-
	%writeln(trying_to_add: H),
	asserta(H),
	%writeln(added_to_kb:H), 
	add_new_evidence(R).

/*
Remove evidence from knowledge base.
*/
remove_evidence([]).
remove_evidence([H|R]) :-
	%writeln(trying_to_remove: H),
	retract(H),
	%writeln(removed_from_kb:H), 
	remove_evidence(R).

/*
has_focus(Action,T2) ~ val(I) :=
	T is T2-1,
	T >= 0,
	action_category(Action,T) ~=Cat,
	is_subcategory(Cat,movesomethings_ca),
	instances ~= I,
	has_category(fluids_ca, I, T) ~= true,
	holds_content(C,I) ~= true,
	on(C,table1,T) ~= true,
	gives_command(H,_,Comm,T),
	focus_constraints(I,Comm) ~=true.

has_focus(Action,T2) ~ val(I) :=
	T is T2-1,
	T >= 0,
	action_category(Action,T) ~=Cat,
	is_subcategory(Cat,movesomethings_ca),
	instances ~= I,
	has_category(moveablethings_ca, I, T) ~= true,
	on(I,table1,T) ~= true,
	gives_command(H,_,Comm,T),
	focus_constraints(I,Comm) ~= true.
*/

/*
	wants_action(human(N)):t ~= B,
	writeln(B),
	findall(Sub,subcategory(Sub,performableactions_ca),L2),
	writeln(L2).
*/
/*
wanted_action(human(N)):t+1 ~ val(WA) :-
	wanted_action(human(N)):t ~= WA.
*/	

/*
wants_action(human(N)):t+1 ~ val(true) :=
	human(N) ~= true,
	action(command_content(command(N),Content)),%TODO link tss human en command
	member(pickup(_),Content).
wants_action(human(N)):t+1 ~ val(true) :=
	human(N) ~= true,
	action(gives_command(human(N),_,Comm)),
	comm_category(Comm):t+1 ~= answerquestioncommands_ca. %TODO hoe doet ge een 1 of 2 ?
wants_action(human(N)):t+1 ~ val(true) :=
	human(N) ~= true,
	action(gives_command(human(N),_,Comm)),
	comm_category(Comm):t+1 ~= performphysicalactioncommands_ca.
*/

/*
action_category(human(N)):0 ~ uniform(L) :=
	human(N) ~= true,
	findall(Sub,subcategory(Sub,performableactions_ca),L). %TODO is_sub
*/

/*
action_category(human(N)):0 ~ val(none) := true.

action_category(human(N)):t+1 ~ val(pickup_ca) :=
	human(N) ~= true,
	action(gives_command(human(N),_,Comm)),
	action(command_content(Comm,Content)),
	member(pickup(_),Content).
	%TODO has_need(human(N)):t ~= obtain_item.
action_category(human(N)):t+1 ~ uniform(L) :=
	human(N) ~= true,
	wants_action(human(N)):t+1 ~= true,
	action_category(human(N)):t ~= none,
	findall(Sub,subcategory(Sub,performableactions_ca),L).
action_category(human(N)):t ~ val(none) :=
	human(N) ~= true,
	wants_action(human(N)):t ~= false.

action_category(human(N)):t+1 ~ val(AC) :=
	action_category(human(N)):t ~= AC.
*/

