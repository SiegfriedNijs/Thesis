
%%% -*- Mode: Prolog; -*-

:- use_module(library(lists)).
:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module('knowledge_base.pl').
:- use_module('knowledge_processing.pl').
:- use_module('ontology.pl').
:- set_magic(backward). 

lifted(false).
raoblackwellisation(false).
%:- style_check(all).

:- set_magic(backward). %NP
:- set_debug(false).
:- dynamic instance2/2.
:- dynamic has_colour/2.
:- dynamic has_position/2.
:- dynamic nocare/2.
:- dynamic care/2.
:- dynamic unknowncare/2.
:- dynamic perf/3.

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
builtin(flatten(_,_)).
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
builtin(has_colour(_,_)).
builtin(has_physical_generalRGB(_,_)).
builtin(get_objects2(_,_,_)).
builtin(append(_,_,_)).
builtin(delete(_,_,_)).
builtin(nth1(_,_,_)).
builtin(perf(_,_,[_,_,_,_,_])).
builtin(getProbList(_,_,_,_)).
builtin(human(_)).
builtin(inside_room(_,_)).
builtin(has_position(_,_)).
builtin(has_category(_,_)).
builtin(maxobjs(_)).
builtin(distance(_,_,_)).
builtin(right_shoulder(_,_,_)).
builtin(left_shoulder(_,_,_)).
builtin(length_arm(_)).
builtin(statement(_,_)).
builtin(get_variables(_,_)).
builtin(flatten(_,_)).
builtin(human_removed(_)).
builtin(human_placed(_)).
builtin(end_perf(_)).
builtin(ingripper).
builtin(fell).
builtin(ontable).
builtin(stillholding).

satisfiable(findall(_,_,_)).
builtin(\+A) :-
	builtin(A).
checkvalue(true,1.0).
checkvalue(false,0.0).

/*
Observation to indicate whether this interpretation exists or not.
*/
observation(exists):t+1 ~ val(true) := true.

/*
Number of objects available in surroundings, van be set certain or uncertain.
*/
nobjs:0 ~ uniform([1,2,3,4,5]) := true.
%nobjs:0 ~ val(3).
nobjs:t+1 ~ val(NO) := 
	nobjs:t ~= NO.
object(N):t ~ val(true) := 
	nobjs:t ~= Nu, 
	between(1,Nu,N).

/*
Simple observation model for number of objects seen in one iteration, able to deal with perception issues like occlusion.
*/
observation(nobjsinit):t+1 ~ uniform(L) :=
	nobjs:t+1 ~= NO,
	findall(X,between(0,NO,X),L).
observation(nobjsit):t+1 ~ uniform(L) :=
	nobjs:t+1 ~= NO,
	findall(X,between(0,NO,X),L).

/*
Whether on object has been seen depends on the probabilistic re-recognition of one.
*/
seen(object(N)):0 ~ val(false) :=
	nobjs:0 ~= Nu,
	between(1,Nu,N).
seen(object(N)):t+1 ~ val(true) :=
	action(seen(object(N))).
seen(object(N)):t+1 ~ val(B) :=
	seen(object(N)):t ~=B.

/*
All fluidholders have a content, with "empty" being consider as content too.
*/
content(N):t ~ val(true) :=
	object(N):t ~= true,
	has_category(fluidholders_ca,object(N)):t ~= true.
content(N):t ~ val(false) := 
	object(N):t ~= true,
	has_category(fluidholders_ca,object(N)):t ~= false.

/*
Probabilistic object categorization after RANSAC recognition action.
*/
obj_category(object(N)):0 ~ val(none) :=
	nobjs:0 ~= Nu, 
	between(1,Nu,N).
obj_category(object(N)):t+1 ~ finite(Distr) :=
	action(segm(object(N),Distr)).
obj_category(object(N)):t+1 ~ val(Val) :=
	action(cat_ev(object(N),Val)).
obj_category(object(N)):t+1 ~ val(Cat) :=
	obj_category(object(N)):t ~= Cat.

/*
Categorization of concept "robot/myself" in ontology.
*/
myself(pr2robot1).
has_category(pr2_ca,pr2robot1).
has_category(Super,pr2robot1) :-
	category(Super),
	is_subcategory(Sub,Super),
	has_category(Sub,pr2robot1).

/*
Categorization of concept "human" in ontology.
*/
human(1).
has_category(humans_ca,human(N)).
has_category(Super,human(N)) :-
	category(Super),
	is_subcategory(Sub,Super), 	
	has_category(Sub,human(N)).

/*
Categorization of concept "table" in ontology.
*/
has_category(tables_ca,table).
has_category(Super,table) :-
	category(Super),
	is_subcategory(Sub,Super),
	has_category(Sub,table).

/*
Categorization (probabilistic) of concept "object" in ontology.
*/
has_category(moveablethings_ca,object(N)):t ~ val(true).
has_category(Cat,object(N)):t ~ val(true) :=
	category(Cat),
	obj_category(object(N)):t ~= Cat.
has_category(Super,object(N)):t ~ val(true) :=
 	category(Super),
	subcategory(Sub,Super), 
	has_category(Sub,object(N)):t ~= true.
has_category(_,object(N)):t ~ val(false).

/*
Categorization (probabilistic) of concept "content" in ontology.
*/
has_category(Category, content(N)):t ~ val(true) :=
	holds_fluid(object(N)):t ~= Category.
has_category(Super,content(N)):t ~ val(true) :=
	category(Super),
	subcategory(Sub,Super), 
	has_category(Sub,content(N)):t ~= true.
has_category(_,content(N)):t ~ val(false).

/*
Knowledge on a certain fluidholder category containing some drink.
*/
holds_fluid(object(N)):t ~ finite([0.5:cola_ca,0.5:nothing]) :=
	has_category(can,object(N)):t ~= true.
holds_fluid(object(N)):t ~ finite([0.3:cola_ca,0.4:water_ca,0.3:nothing]) :=
	has_category(cup,object(N)):t ~= true.
holds_fluid(object(N)):t ~ finite([0.5:water_ca,0.5:nothing]) :=
	has_category(mug,object(N)):t ~= true.
holds_fluid(object(N)):t ~ finite([0.5:nondrinkablefluid_ca,0.5:nothing]) :=
	has_category(nondrinking_bottle,object(N)):t ~= true.
holds_fluid(object(N)):t+1 ~ val(Val) :=
	holds_fluid(object(N)):t ~= Val.

/*
Human needs, attention model. Extension for user action recognition.
*/
/*
attention_towards(human(N)):0 ~ val(nothing).
attention_towards(human(N)):t+1 ~ val(O) :=
	action(attention(O)).
attention_towards(human(N)):t+1 ~ val(O) :=
	attention_towards(human(N)):t ~= O.
*/

/*
All humans are inside the kitchen for the experiment.
*/
inside_room(human(N),kitchen) :-
	human(N).

/*
Chances of a user having a certain craving, given the room the user is in.
*/
has_craving(human(1),thirst):t+1 ~ finite([0.9: true, 0.1:false]) :=
	action(cravingbegin(human(1))),
	inside_room(human(1),kitchen).
has_craving(human(1),thirst):t+1 ~ finite([0.9: false, 0.1:true]) :=
	action(cravingbegin(human(1))),	
	inside_room(human(1),bedroom). 
has_craving(human(1),hunger):t+1 ~ finite([0.5: true, 0.5:false]) :=
	action(cravingbegin(human(1))),
	inside_room(human(1),kitchen).
has_craving(human(N),Craving):t+1 ~ val(C) :=
	has_craving(human(N),Craving):t ~= C.

/*
The real intention or need of a user is depending on his or her cravings.
*/
has_need(human(N)):t+1 ~ finite([0.3:obtain_item(drinkablefluids_ca),0.3:obtain_item(eatingtools_ca),0.2:obtain_item(moveablethings_ca),0.1:cleanup,0.1:info]) :=
	action(needbegin(human(N))),
	human(N),
	inside_room(human(N),kitchen),
	has_craving(human(N),thirst):t ~= true,
	has_craving(human(N),hunger):t ~= true.
has_need(human(N)):t+1 ~ finite([0.6:obtain_item(drinkablefluids_ca),0.2:obtain_item(moveablethings_ca),0.1:cleanup,0.1:info]) :=
	action(needbegin(human(N))),
	human(N),
	inside_room(human(N),kitchen),
	has_craving(human(N),thirst):t ~= true.
has_need(human(N)):t+1 ~ finite([0.6:obtain_item(eatingtools_ca),0.2:obtain_item(moveablethings_ca),0.1:cleanup,0.1:info]) :=
	action(needbegin(human(N))),
	human(N),
	inside_room(human(N),kitchen),
	has_craving(human(N),hunger):t ~= true.
has_need(human(N)):t+1 ~ finite([0.7:obtain_item(moveablethings_ca),0.3:info]) :=
	action(needbegin(human(N))),
	human(N),
	inside_room(human(N),kitchen).
has_need(human(N)):t+1 ~ finite([0.9:info,0.1:obtain_item(moveablethings_ca)]) := 
	action(needbegin(human(N))),
	human(N),
	inside_room(human(N),testroom).
has_need(human(N)):t+1 ~ uniform([obtain_item(moveablethings_ca),info]) :=
	action(needbegin(human(N))),
	human(N).	
has_need(human(N)):t+1 ~ val(Need) :=
	has_need(human(N)):t ~= Need.


/*
Care model section.
Once a user indicates an action is wanted we attach a prior chance to the wa distribution of the user not caring about a certain choice.
*/
care(human(N),type):t+1 ~ finite([0.9:care,0.1:nocare]) :=
	human(N),
	action(wabegin).
care(human(N),object):t+1 ~ finite([0.9:care,0.1:nocare]) :=
	human(N),
	action(wabegin).
care(human(N),tool):t+1 ~ finite([0.3:care,0.7:nocare]) :=
	human(N),
	action(wabegin).
care(human(N),T):t+1 ~ val(Bool) :=
	care(human(N),T):t ~= Bool.

/*
Calculation of all possible wanted actions based on the probabilistic need.
*/
wanted_action(human(N)):t+1 ~ uniform(Pr3) :=
	human(N), 
	action(wabegin),
	has_need(human(N)):t ~= obtain_item(drinkablefluids_ca),
	performable(pickup_ca):t ~= Lpickup,
	performable(place_ca):t ~= Q,
	pruneFocusType(Lpickup,fluidholders_ca):t ~= Pruned,
	append(Q,Pruned,Pr2),
	performable(answerquestion_ca):t ~= Q2,
	append(Pr2,Q2,Pr3).
wanted_action(human(N)):t+1 ~ uniform(Pr3) :=
	human(N),
	action(wabegin),
	has_need(human(N)):t ~= obtain_item(Cat),
	performable(pickup_ca):t ~= Lpickup,
	pruneFocusType(Lpickup,Cat):t ~= Pruned,
	performable(place_ca):t ~= Q,
	append(Q,Pruned,Pr2),
	performable(answerquestion_ca):t ~= Q2,
	append(Pr2,Q2,Pr3).
wanted_action(human(N)):t+1 ~ uniform(Cl2) :=
	human(N),
	action(wabegin),
	has_need(human(N)):t ~= cleanup,
	performable(pickup_ca):t ~= Lpickup,
	performable(place_ca):t ~= Lplace,
	performable(answerquestion_ca):t ~= Q,
	append(Lpickup,Lplace,Cl),
	append(Cl,Q,Cl2).
wanted_action(human(N)):t+1 ~ uniform(Q) :=
	human(N),
	action(wabegin),
	has_need(human(N)):t ~= info,
	performable(answerquestion_ca):t ~= Q.
wanted_action(human(N)):t+1 ~ uniform(L2) := %general wanted_action without need context
	human(N),
	action(wabegin),
	performable(pickup_ca):t ~= Lpickup,
	performable(place_ca):t ~= Lplace,
	performable(answerquestion_ca):t ~= Q,
	append(Lplace,Lpickup,L1),
	append(L1,Q,L2).
wanted_action(human(N)):t+1 ~ val(nothing_possible) :=
	human(N),
	action(wabegin),
	performable(pickup_ca):t ~= Lpickup,
	performable(place_ca):t ~= Lplace,
	append(Lplace,Lpickup,R),
	R=[].
wanted_action(human(N)):t+1 ~ val(B) :=
	wanted_action(human(N)):t ~= B.

/*
A positive feedback on a performed action (from the user) can only occur if it was a wanted action.
*/
observation(posfeedback(human(N))):t+1 ~ val(WA) :=
	wanted_action(human(N)):t+1 ~= WA.

/*
Extra HRI vocabulary, coupling the statement to the implications for the wanted action.
*/
wants_to_move(human(N),object(O)):t ~ val(true) :=
	wanted_action(human(N)):t ~= (pickup,object(O),_).
wants_to_move(human(N),object(O)):t ~ val(true) :=
	wanted_action(human(N)):t ~= (place,object(O),_).
wants_to_move(human(N),object(O)):t ~ val(false).

wants_pickup(human(N),object(O)):t ~ val(true) :=
	wanted_action(human(N)):t ~= (pickup,object(O),_).
wants_pickup(human(N),object(O)):t ~ val(false).

has_focus(human(N)):t ~ val(Theme) :=
	wanted_action(human(N)):t ~= (_,Theme,_).

wants_tool_used(human(N)):t ~ val(Tool) :=
	wanted_action(human(N)):t ~= (_,_,Tool).

wants_object(Cat,human(N)):t ~ val(true) :=
	wanted_action(human(N)):t ~= (pickup,object(O),_),
	has_category(Cat,object(O)):t ~= true.
wants_object(Cat,human(N)):t ~ val(false).

wants_info_on(human(N),object(O)):t ~ val(true) :=
	wanted_action(human(N)):t ~= (info,object(O),_).
wants_info_on(human(N),object(O)):t ~ val(false).

wants_drinkablefluid(human(N)):t ~ val(true) :=
	wanted_action(human(N)):t ~= (pickup,object(O),_),
	content(O):t ~= true,
	has_category(drinkablefluids_ca,content(O)):t ~=true.
wants_drinkablefluid(human(N)):t ~ val(false).

wants_water(human(N)):t ~ val(true) :=
	wanted_action(human(N)):t ~= (pickup,object(O),_),
	content(O):t ~= true,
	has_category(water_ca,content(O)):t ~=true.
wants_water(human(N)):t ~ val(false).

cleanup(table):t ~ val(true) :=
	wanted_action(human(N)):t ~= (pickup,object(O),_),
	content(O):t ~= true,
	holds_fluid(object(O)):t ~= nothing.

/*
Pruning of WA list based on object type.
*/
pruneFocusType([],_):t ~ val([]) := true.
pruneFocusType([(AT,F,Params)|R],C):t ~ val([(AT,F,Params)|R2]) :=
	has_category(C,F):t ~= true,
	pruneFocusType(R,C):t ~= R2.
pruneFocusType([(AT,F,Params)|R],C):t ~ val(R2) :=
	has_category(C,F):t ~= false,
	pruneFocusType(R,C):t ~= R2.
pruneFocusType([(AT,F,Params)|R],C):t ~ val([(AT,F,Params)|R2]) :=
	has_category(C,F),
	pruneFocusType(R,C):t ~= R2.
pruneFocusType([(AT,F,Params)|R],C):t ~ val(R2) :=
	\+has_category(C,F),
	pruneFocusType(R,C):t ~= R2.
	
pruneFocusID([],_):t ~ val([]) := true.
pruneFocusID([(AT,F,Params)|R],ID):t ~ val([(AT,F,Params)|R2]) :=
	F == ID,
	pruneFocusID(R,ID):t ~= R2.
pruneFocusID([(AT,F,Params)|R],ID):t ~ val(R2) :=
	pruneFocusID(R,ID):t ~= R2.

/*
Calculation of possible and performable pickups given current surroundings.
*/
performable(pickup_ca):t ~ val(L) :=
	findall_forward(
	(pickup,object(ID),Gripper),
	(
	availablemeans(pickup_ca):t ~= Gripper,
	current(on(object(ID),table)) ~= true,
	current(reachable(object(ID),Gripper))~= true	
	)
	,L).

/*
Calculation of possible and performable place actions given current surroundings.
*/
performable(place_ca):t ~ val(L) :=
	findall_forward((place,Obj,X),
	(current(holding_object(X)) ~= Obj,
	Obj \= none),L). 

/*
Calculation of possible and performable info actions given current surroundings.
*/
performable(answerquestion_ca):t ~ val(L) :=
	findall_forward((info,object(Obj),robotbrain), 
	(current(object(Obj)) ~= true,
	current(seen(object(Obj))) ~= true)
	,L).

/*
List of grippers not holding anuthing.
*/
availablemeans(pickup_ca):t ~ uniform(Means) :=
	findall_forward(X,current(holding_object(X)) ~= none,Means).

/*
Model of the two grippers holding an object, probabilistic depending on uncertain action effect.
*/
holding_object(left_gripper):0 ~ val(none) := true.
holding_object(right_gripper):0 ~ val(none) := true.

holding_object(Gripper):t+1 ~ val(O) := 
	action(end_perf(PName)),
	perf(PName,pickup,[O,_,_,_,Gripper]),
	effect(PName):t ~= ingripper.
holding_object(Gripper):t+1 ~ val(none) := 
	action(end_perf(PName)),
	perf(PName,pickup,[O,_,_,_,Gripper]),
	effect(PName):t ~= fell.
holding_object(Gripper):t+1 ~ val(none) := 
	action(end_perf(PName)),
	perf(PName,pickup,[O,_,_,_,Gripper]),
	effect(PName):t ~= ontable.
holding_object(Gripper):t+1 ~ val(none) := 
	holding_object(Gripper):t ~= O,	
	action(end_perf(PName)),
	perf(PName,place,[O,_,_,_,Gripper]),
	effect(PName):t ~= ontable.
holding_object(Gripper):t+1 ~ val(none) := 
	holding_object(Gripper):t ~= O,	
	action(end_perf(PName)),
	perf(PName,place,[O,_,_,_,Gripper]),
	effect(PName):t ~= fell.
holding_object(Gripper):t+1 ~ val(O) := 
	holding_object(Gripper):t ~= O,	
	action(end_perf(PName)),
	perf(PName,place,[O,_,_,_,Gripper]),
	effect(PName):t ~= stillholding.
holding_object(Gripper):t+1 ~ val(X) :=
	holding_object(Gripper):t ~= X.

/*
Robot location self-knowledge, reachable common sense.
*/
right_shoulder(0,0.2,1).
left_shoulder(0,-0.2,1).
length_arm(0.9).

gripper(left_gripper).
gripper(right_gripper).

reachable(object(N),Gr):t ~ finite([1:false]) :=
	has_position(object(N)):t ~= (X,Y,Z),
	length_arm(LA),	
	left_shoulder(XLS,YLS,ZLS),
	distance((X,Y,Z),(XLS,YLS,ZLS),Distance),
	Distance > LA.
reachable(object(N),Gr):t ~ finite([0.95:true,0.05:false]) :=
	has_position(object(N)):t ~= (X,Y,Z),
	length_arm(LA),	
	Treshold is LA - 0.2,
	left_shoulder(XLS,YLS,ZLS),
	distance((X,Y,Z),(XLS,YLS,ZLS),Distance),
	Distance < Treshold.
reachable(object(N),Gr):t ~ finite([0.50:true,0.50:false]) := 
	has_position(object(N)):t ~= (X,Y,Z),
	length_arm(LA),	
	Treshold is LA - 0.2,
	left_shoulder(XLS,YLS,ZLS),
	distance((X,Y,Z),(XLS,YLS,ZLS),Distance),
	Distance > Treshold,
	Distance < LA.

/*
Performance approval.
*/
%good_perform:0 ~ val(none) := true.
good_perform(Perf):t+1 ~ val(true) :=
	action(start_perf(Perf)),
	perf(Perf,Action,Params),
	member(Action,[toolq,typeq,objectq]).
good_perform(Perf):t+1 ~ uniform([true,false]) :=
	action(start_perf(Perf)).
good_perform(Perf):t+1 ~ val(true) :=
	action(feedback(Perf,good)).
good_perform(Perf):t+1 ~ val(false) :=
	action(feedback(Perf,bad)).
good_perform(Perf):t+1 ~ val(B) :=
	good_perform(Perf):t ~=B.

/*
Subcategory transitivity.
*/

is_subcategory(Category1,Category2) :-
	findall(X,subcategory(X,Category2),List),
	length(List,Length),
	Length =\= 0,
	is_subcategory2(Category1,List).
is_subcategory(Category1,Category2) :-
	subcategory(Category1,Category2).
is_subcategory2(Category1,[H|R]) :-
	is_subcategory(Category1,H).
is_subcategory2(Category1,[H|R]) :-
	is_subcategory2(Category1,R).

/*
Position gaussian observation model.
*/
observation(object(N)):t+1 ~ indepGaussians([ ([X],[0.005]), ([Y],[0.005]), ([Z],[0.005]) ]) :=
	has_position(object(N)):t+1 ~= (X,Y,Z).

/*
Probabilistic effects of each action.
*/
effect(PName):t+1 ~ finite([0.9:ingripper,0.05:fell,0.05:ontable]) :=
	%writeln(effect1),
	action(start_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	obj_category(object(N)):t~=can.
effect(PName):t+1 ~ finite([0.5:ontable,0.4:fell,0.1:stillholding]) :=
	%writeln(effect2),
	action(start_perf(PName)),
	perf(PName,place,[object(N),_,_,_,Gripper]),
	obj_category(object(N)):t~=can.

effect(PName):t+1 ~ finite([0.8:ingripper,0.1:fell,0.1:ontable]) :=
	%writeln(effect3),	
	action(start_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	obj_category(object(N)):t~=cup.	
effect(PName):t+1 ~ finite([0.50:ontable,0.45:fell,0.05:stillholding]) :=
	%writeln(effect4),	
	action(start_perf(PName)),
	perf(PName,place,[object(N),_,_,_,Gripper]),
	obj_category(object(N)):t~=cup.

effect(PName):t+1 ~ finite([0.5:ingripper,0.3:fell,0.2:ontable]) :=
	%writeln(effect5),	
	action(start_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	obj_category(object(N)):t~=nondrinking_bottle.
effect(PName):t+1 ~ finite([0.35:ontable,0.6:fell,0.05:stillholding]) :=
	%writeln(effect6),	
	action(start_perf(PName)),
	perf(PName,place,[object(N),_,_,_,Gripper]),
	obj_category(object(N)):t~=nondrinking_bottle.

effect(PName):t+1 ~ finite([0.1:ingripper,0.4:fell,0.5:ontable]) :=
	%writeln(effect7),	
	action(start_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	obj_category(object(N)):t~=mug.
effect(PName):t+1 ~ finite([0.15:ontable,0.8:fell,0.05:stillholding]) :=
	%writeln(effect8),	
	action(start_perf(PName)),
	perf(PName,place,[object(N),_,_,_,Gripper]),
	obj_category(object(N)):t~=mug.

effect(PName):t+1 ~ uniform([ingripper,fell,ontable]) :=	
	action(start_perf(PName)),	
	%writeln(effect9),	
	perf(PName,pickup,[object(N),_,_,_,Gripper]).
effect(PName):t+1 ~ uniform([ontable,fell,stillholding]) :=
	%writeln(effect10),	
	action(start_perf(PName)),
	perf(PName,place,[object(N),_,_,_,Gripper]).
effect(PName):t+1 ~ val(Effect) :=
	effect(PName):t ~= Effect.

/*
Indicates whether an action effect is wanted or not in general.
*/
wanted_effect(PName):t ~ val(true) := %question action always has a wanted effect
	action(start_perf(PName)),
	perf(PName,Action,_),
	member(Action,[toolq,typeq,objectq]).
wanted_effect(PName):t ~ val(true) :=
	action(end_perf(PName)),
	perf(PName,info,[_,_,_,_,_]).
wanted_effect(PName):t ~ val(true) :=
	action(end_perf(PName)),
	perf(PName,pickup,[_,_,_,_,_]),
	effect(PName):t ~= ingripper.
wanted_effect(PName):t ~ val(false) :=
	action(end_perf(PName)),
	perf(PName,pickup,[_,_,_,_,_]),
	effect(PName):t ~= fell.
wanted_effect(PName):t ~ val(false) :=
	action(end_perf(PName)),
	perf(PName,pickup,[_,_,_,_,_]),
	effect(PName):t ~= ontable.
wanted_effect(PName):t ~ val(true) :=
	action(end_perf(PName)),
	perf(PName,place,[_,_,_,_,_]),
	effect(PName):t ~= ontable.
wanted_effect(PName):t ~ val(false) :=
	action(end_perf(PName)),
	perf(PName,place,[_,_,_,_,_]),
	effect(PName):t ~= fell.
wanted_effect(PName):t ~ val(false) :=
	action(end_perf(PName)),
	perf(PName,place,[_,_,_,_,_]),
	effect(PName):t ~= stillholding.
wanted_effect(PName):t+1 ~ val(Effect) :=
	wanted_effect(PName):t ~= Effect.

/*
Feedback that effect was wanted by user only for interpretations where predicted effect is wanted.
*/
observation(wanted_feedback(PName)):t+1 ~ val(B) :=
	wanted_effect(PName):t+1 ~= B.

/*
Object and gripper positions.
*/	
has_position(left_gripper,[0,0,0]).
has_position(right_gripper,[0,0,0]).

has_position(object(N)):0 ~ finite([1:(-1000,-1000,-1000)]) := 
	nobjs:0 ~= Nu,
	between(1,Nu,N).
has_position(object(N)):t+1 ~ finite([1:(-1000,-1000,-1000)]) :=
	action(human_removed(object(N))).
has_position(object(N)):t+1 ~ finite([1:(X,Y,Z)]) :=
	action(human_placed(object(N),[X,Y,Z])).	
has_position(object(N)):t+1 ~ finite([1:(X,Y,Z)]) :=
	action(pos(object(N),[X,Y,Z])).	
	
/*
Effects op pickup action on object position.
*/
has_position(object(N)):t+1 ~ finite([1:(X2,Y2,Z2)]) :=
	action(end_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	effect(PName):t ~= ingripper,
	has_position(Gripper,[X2,Y2,Z2]).
has_position(object(N)):t+1 ~ finite([1:(X,Y,Z)]) := 
	action(end_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	effect(PName):t ~= ontable,
	has_position(object(N)):t ~=(X,Y,Z).
has_position(object(N)):t+1 ~ finite([1:(-1000,-1000,-1000)]) :=
	action(end_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	effect(PName):t ~= fell.

/*
Effects op place action on object position.
*/
has_position(object(N)):t+1 ~ finite([1:(-1000,-1000,-1000)]) :=
	action(end_perf(PName)),
	perf(PName,place,[object(N),_,_,_,_]),
	effect(PName):t ~= fell.
has_position(object(N)):t+1 ~ finite([1:(X2,Y2,Z2)]) :=
	action(end_perf(PName)),
	perf(PName,place,[object(N),X2,Y2,Z2,Gripper]),
	effect(PName):t ~= ontable.
has_position(object(N)):t+1 ~ finite([1:(X2,Y2,Z2)]) :=
	action(end_perf(PName)),
	perf(PName,place,[object(N),_,_,_,Gripper]),
	effect(PName):t ~= stillholding,
	has_position(Gripper,[X2,Y2,Z2]).

has_position(object(N)):t+1 ~ finite([1:(X,Y,Z)]) :=
	has_position(object(N)):t ~= (X,Y,Z).

has_position(table):0 ~ finite([1:(0,0,0)]) := true.	
has_position(table):t+1 ~ finite([1:(X,Y,Z)]) :=
	action(pos(table,[X,Y,Z])).	
has_position(table):t+1 ~ finite([1:(X,Y,Z)]) :=
	has_position(table):t ~= (X,Y,Z).

/*
Probabilistic dimensions, since experiments are possible with uncertain object IDs.
*/
has_dimensions(object(N)):0 ~ val([0.1,0.1,0.1]) :=
	nobjs:0 ~= Nu,
	between(1,Nu,N).
has_dimensions(object(N)):t+1 ~ val(Dims) :=
	action(objdims(object(N),Dims)).
has_dimensions(object(N)):t+1 ~ val(Dims) :=
	has_dimensions(object(N)):t ~= Dims.

has_dimensions(table):0 ~ finite([1:(0,0,0)]) := true.
has_dimensions(table):t+1 ~ finite([1:[X,Y,Z]]) :=
	action(dim(table,[X,Y,Z])).
has_dimensions(table):t+1 ~ finite([1:[X,Y,Z]]) :=
	has_dimensions(table):t ~= [X,Y,Z].

/*
Defines when an object is near another one.
*/
near(O,O):t ~ finite([1:true]) := true.
near(O1,O2):t ~ finite([1:true]) :=
	has_position(O1):t ~= (X1,Y1,Z1),
	has_position(O2):t ~= (X2,Y2,Z2),
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],

	X1min is X1 - (Xrange1/2),
	X1max is X1 + (Xrange1/2),
	X2min is X2 - (Xrange2/2),
	X2max is X2 + (Xrange2/2),
	distance_interval([X1min,X1max],[X2min,X2max],DistX),
	DistX < 0.1,

	Y1min is Y1 - (Yrange1/2),
	Y1max is Y1 + (Yrange1/2),
	Y2min is Y2 - (Yrange2/2),
	Y2max is Y2 + (Yrange2/2),
	distance_interval([Y1min,Y1max],[Y2min,Y2max],DistY),
	DistY < 0.1,

	Z1min is Z1 - (Zrange1/2),
	Z1max is Z1 + (Zrange1/2),
	Z2min is Z2 - (Zrange2/2),
	Z2max is Z2 + (Zrange2/2),
	distance_interval([Z1min,Z1max],[Z2min,Z2max],DistZ),
	DistZ < 0.1.
near(O1,O2):t ~ val(false) := true.

/*
Definition of an object being on another one at time T.
*/
on(O,O):t ~ finite([1:false]) := true.
on(O1,O2):t ~ finite([1:true]) :=
		has_position(O1):t ~= (X1,Y1,Z1),
		has_position(O2):t ~= (X2,Y2,Z2),
		has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
		has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
		Xlow is X2-(Xrange2/2),
		Xhigh is X2 + (Xrange2/2),
		X1 > Xlow,
		X1 < Xhigh,
		Ylow is Y2-(Yrange2/2),
		Yhigh is Y2 + (Yrange2/2),
		Y1 > Ylow,
		Y1 < Yhigh,
		ZZ is Z2 + (Zrange1/2),
		
		ZZ2 is ZZ + (Zrange2/2),
		Zdiff is abs(Z1-ZZ2),
		Zdiff < 0.03. 
on(O1,O2):t ~ finite([1:false]) := true.

/*
Definition of an object being left of another one at time T.
*/
leftof(O,O):t ~ val(false) := 
	true.
leftof(O1,O2):t ~ val(true) :=
	has_position(O1):t ~= (X1,Y1,Z1),
	has_position(O2):t ~= (X2,Y2,Z2),
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
	Y1max is Y1 + (Yrange1/2),
	Y2min is Y2 - (Yrange2/2),
	Y1max < Y2min. 
leftof(O1,O2):t ~ val(false) := 
	true.

/*
Definition of an object being right of another one at time T.
*/
rightof(O,O):t ~ val(false) := true.
rightof(O1,O2):t ~ finite([1:true]) :=
	has_position(O1):t ~= (X1,Y1,Z1),
	has_position(O2):t ~= (X2,Y2,Z2),
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
	Y1min is Y1 - (Yrange1/2),
	Y2max is Y2 + (Yrange2/2),
	Y1min > Y2max.
rightof(O1,O2):t ~ finite([1:false]) := true.

/*
Definition of an object being in front of another one at time T.
*/
frontof(O,O):t ~ val(false) := true.
frontof(O1,O2):t ~ finite([1:true]) :=
	has_position(O1):t ~= (X1,Y1,Z1),
	has_position(O2):t ~= (X2,Y2,Z2),
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
	X1max is X1 + (Xrange1/2),
	X2min is X2 - (Xrange2/2),
	X1max < X2min. 
frontof(O1,O2):t ~ finite([1:false]) := true.

/*
Definition of an object being back of another one at time T.
*/
backof(O,O):t ~ val(false) := true.
backof(O1,O2):t ~ finite([1:true]) :=
	has_position(O1):t ~= (X1,Y1,Z1),
	has_position(O2):t ~= (X2,Y2,Z2),
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
	X1min is X1 - (Xrange1/2),
	X2max is X2 + (Xrange2/2),
	X1min > X2max.  
backof(O1,O2):t ~ finite([1:false]) := true.

/*
Definition of an object being bigger than another one at time T.
*/
bigger(O,O):t ~ val(false) := true.
bigger(O1,O2):t ~ val(true) :=
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
	Zrange1 > Zrange2.
bigger(O1,O2):t ~ val(false) := true.

/*
Definition of an object being smaller than another one at time T.
*/
smaller(O,O):t ~ val(false) := true.
smaller(O1,O2):t ~ val(true) :=
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
	Zrange1 < Zrange2.
smaller(O1,O2):t ~ val(false) := true.

/*
Definition of an object being thinner than another one at time T.
*/
thinner(O,O):t ~ val(false) := true.
thinner(O1,O2):t ~ val(true) :=
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
	Xrange1 < Xrange2,
	Yrange1 < Yrange2.
thinner(O1,O2):t ~ val(false) := true.

/*
Definition of an object being wider than another one at time T.
*/
wider(O,O):t ~ val(false) := true.
wider(O1,O2):t ~ val(true) :=
	has_dimensions(O1):t ~= [Xrange1,Yrange1,Zrange1],
	has_dimensions(O2):t ~= [Xrange2,Yrange2,Zrange2],
	Xrange1 > Xrange2,
	Yrange1 > Yrange2.
wider(O1,O2):t ~ val(false) := true.

/*
Robot self-knowledge.
*/
possible_actions(pr2_ca,[pickup_ca,pickandplace_ca]).
generate_content_name(O,C) :-
	atomic_concat([O,content],C).

/*
Add new evidence to knowledge base.
*/
add_new_evidence([]).
add_new_evidence([H|R]) :-
	%writeln(trying_to_add: H),
	asserta(H),
	%writeln(added_to_kb:H), 
	add_new_evidence(R).
add_evidence(E) :-
	asserta(E).

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
Processing of probability lists.
*/
getProbList(Cat,L2,CatProb,ProbList) :-
	Begin = [CatProb:Cat],
	length(L2,Length),

	(Length == 1 ->
		ProbList = [1:Cat]
	;
		delete(L2, Cat, Rest),
		length(Rest,Length2),
		N2 is (1 - CatProb), 
		Prob2 is N2/Length2,
		getProbList2(Prob2,Rest,Result),
		append(Begin,Result,ProbList)
	).

getProbList2(_,[],[]).
getProbList2(Prob2,[H|R],[(Prob2:H)|R2]) :-
	getProbList2(Prob2,R,R2).

/*
Extra static robot self-knowledge.
*/
essentialparameters(pickup_ca,[object_pickedup(pickup_ca, moveablethings_ca), has_performer(pickup_ca, actionperformers_ca), uses_gripper(pickup_ca, robotarms_ca)]).
acts_on_object(pickup_ca, moveablethings_ca).
pickup(pickup_ca, spatialobjects_ca).
can_perform(pr2_ca,pickup_ca).
maxobjs(5).

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

precondition_focus_prune(pickup_ca,[]):t ~ val([]) := true.
precondition_focus_prune(pickup_ca,[H|R]):t ~ val([H|R2]) :=
	has_category(moveablethings_ca, H):t ~= true,
	on(H,table):t ~= true, 
	precondition_focus_prune(pickup_ca,R):t ~= R2.
precondition_focus_prune(pickup_ca,[content(N)|R]):t ~ val([content(N)|R2]) :=
	content(N):t ~= true,
	has_category(fluids_ca, content(N)):t ~= true,
	on(object(N),table):t ~= true, 
	precondition_focus_prune(pickup_ca,R):t ~= R2.
precondition_focus_prune(pickup_ca,[H|R]):t ~ val(R2) :=
	precondition_focus_prune(pickup_ca,R):t ~= R2.
precondition_focus_prune(place_ca,[]):t ~ val([]) := true.
precondition_focus_prune(place_ca,[H|R]):t ~ val([H|R2]) :=
	has_category(moveablethings_ca, H):t ~= true,
	on(H,table):t ~= false, 
	precondition_focus_prune(place_ca,R):t ~= R2.
precondition_focus_prune(place_ca,[H|R]):t ~ val(R2) :=
	precondition_focus_prune(place_ca,R):t ~= R2.

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
	wants_action(H,A,T) ~= true,
	has_category(performableactions_ca,A,T) ~= true,
	myself(M).
has_performer(A,T) ~ uniform(L) :=
	has_category(actions_ca,A,T) ~= true,
	findall(X,has_category(actionperformers_ca,X,T),L).

has_start(A,T) ~ val(onsurface) := 
	wants_action(H,A,T) ~= true, 
	has_category(movesomethings_ca,A,T) ~= true,
	%in kitchen
	workingplace(Surface).

has_performer(pickup_ca, actionperformers_ca). 
focus_constraints(_,_):t ~ val(false) :=
	true.

/*
Extension: Categorization of commands, future work.
*/

/*
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
*/


/*
wants_action(human(N)):0 ~ val(false) := 	
	human(N).
wants_action(human(N)):t+1 ~ val(true) :=
	human(N),
	action(actiondemand(human(N))).
wants_action(human(N)):t+1 ~ val(B) :=
	human(N),
	wants_action(human(N)):t ~= B. 
*/

/*
wanted_action(human(N)):t+1 ~ uniform(Performable) :=

	human(N),
	action(wabegin),
	attention_towards(human(N)):t ~= At,
	At \= nothing,
	performable(pickup_ca):t ~= Lpickup,
	performable(place_ca):t ~= Lplace,
	performable(answerquestion_ca):t ~= Q,
	append(Lpickup,Lplace,R1),

	append(R1,Q,R2),
	pruneFocusID(R2,At):t ~= Performable,
	writeln(Performable).
*/

%%%%%%%%%%%%%%
%FUTURE IDEAS%
%%%%%%%%%%%%%%
/*
has_craving(human(N),Craving):t+1 ~ uniform([true,false]) :=
	action(needbegin(human(N))),	
	subcategory(Craving,cravings_ca).
*/
/*
has_category(Sub,object(N)):t ~ val(true) :=
	%writeln(test3),
	%writeln(Super),
	category(Sub),
	%writeln(ok1),
	is_subcategory(Sub,Super), %NP kan sneller
	%writeln(Sub),	
	has_category(Super,object(N)):t ~= true.
*/
/*
has_category(Cat,object(N)):t+1 ~ val(true) :=
	%writeln(test1),
	category(Cat),
	object(N):t ~= true,
	action(segm(object(N),_)),
	obj_category(object(N)):t+1 ~= Cat.
has_category(Super,object(N)):t+1 ~ val(true) :=
	%writeln(test2),
	object(N):t ~= true,
	category(Super),
	action(segm(object(N),_)),
	obj_category(object(N)):t+1 ~= Cat,
	is_subcategory(Cat,Super),
	is_subcategory(Super,moveablethings_ca).
*/
/*
has_category(Cat,H):t+1 ~ val(B) :=
	has_category(Cat,H):t ~= B.
*/


/*
has_start(Action,T) ~ val((Start)) :=
	action_category(Action,T) ~= pickup_ca,
	has_focus(Action,T) ~= Object,
	has_position(Object,T) ~= Start.
has_start(Action,T) ~ val((Start)) :=
	action_category(Action,T) ~= pickup_ca,
	has_focus(Action,T) ~= Content,
	holds_content(O,Content) ~= true,
	has_position(O,T) ~= Start.
has_start(Action,T) ~ val((Start)) :=
	action_category(Action,T) ~= answerquestionactions_ca,
	myself(Start).
has_start(Action,T) ~ val((none)) :=
	has_focus(Action,T) ~= X.
*/
/*
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
*/
/*
get_objects:t ~ val(L) :=
	nobjs:t ~= NO,
	get_objects2(1,NO,L).

get_objects2(NO,NO,[object(NO)]).
get_objects2(N,NO,[object(N)|L]) :-
	N<NO,
	N2 is N+1,
	get_objects2(N2,NO,L).
*/
/*
Sample position on table.
*/
%tablepos(Object) ~ gaussian([50,25],[10,0,0,5]) := true.

/*
Sample position on table near an object at a given time.

*/
/*
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
*/
/*
True if object O has been moved from time T to T+1.
*/

/*
Relation inheritance.
*/
/*
has_category(Content) ~ finite([1:ContCat]) := 
				holds_content(Cup,Content)~=true,
				has_category(Cup) ~= cups_ca,
				cupcontentcat(Cup) ~= ContCat.
*/
/*
Container content.
*/
/*
holds_content(object1,object1content) ~ finite([1:true]) := true.
holds_content(object2,object2content) ~ finite([1:true]) := true.
holds_content(object3,object3content) ~ finite([1:true]) := true.
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
*/
%cups hold water, cola or milk
/*
has_category(Content) ~ finite([0.4:water_ca,0.3:cola_ca:0.3:milk_ca]) := 
	holds_content(Container,Content) ~= true,
	has_category(Container) ~= cups_ca.
*/
%red cups only hold cola
/*
has_category(Content) ~ finite([1:cola_ca]) := 
	holds_content(Container,Content) ~= true,
	has_category(Container) ~= cups_ca,
	has_colour(Container) ~= red.
*/
%blue cups only hold water
/*
has_category(Content) ~ finite([1:water_ca]) := 
	holds_content(Container,Content) ~= true,
	has_category(Container) ~= cups_ca,
	has_colour(Container) ~= blue.
*/
%white cups only hold milk
/*
has_category(Content) ~ finite([1:milk_ca]) := 
	holds_content(Container,Content) ~= true,
	has_category(Container) ~= cups_ca,
	has_colour(Container) ~= white.	
*/	
%cup is red, blue or white
/*
has_colour(Cup) ~ finite([0.8:red,0.1:blue,0.1:white]) := 
	has_category(Cup)~= cups_ca.
*/
%table brown or black
/*
has_colour(Table) ~ finite([0.8:brown,0.2:black]) := 
	has_category(Table)~= tables_ca.
*/

/*
Human preference models.
*/
/*
commandcat(human1) ~ finite([0.9:performphysicalactioncommands_ca,0.1:answerquestioncommands_ca]) := 	true.
commandcat(human2) ~ finite([0.2:performphysicalactioncommands_ca,0.8:answerquestioncommands_ca]) := 	true.
commandcat(unknownhuman) ~ finite([0.5:performphysicalactioncommands_ca,0.5:answerquestioncommands_ca]) := true.

objectaction(human1) ~ finite([0.6:pickup_ca,0.4:pickandplace_ca]) := true.
objectaction(human2) ~ finite([0.1:pickup_ca,0.9:pickandplace_ca]) := true.
objectaction(unknownhuman) ~ finite([0.5:pickup_ca,0.5:pickandplace_ca]) := true.
*/
/*
Action models.
*/
/*
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
*/
%actioncategory(A) ~ finite([0.5:pickup_ca,0.5:pickandplace_ca]) := true.
/*
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
*/
/*
Object properties.
*/
/*
has_object_property(O) ~ val(C) := has_colour(O) ~= C.
has_object_property(O) ~ val(M) := has_material(O) ~= M.
has_object_property(O) ~ val(W) := has_weight(O) ~= W.
has_object_property(O) ~ val(D) := has_dimensions(O) ~= D.

object_pick_up(A) ~ finite([1:O]) :=
	has_category(O)~= Cat,
	is_subcategory(Cat,graspablethings_ca).
*/
/*
comm_category(command(N)):t+1 ~ val(answerquestioncommands_ca) :=

	command(N) ~= true,
	action(gives_command(H,R,I)),
	ask_question(M,Q,T),
	implies_choice(Q,_,_,Choice),
	command_content(I,Content),
	nth1(1,Content,C1),
	member(C1,Choice).
*/

/*
waiting_answer:0 ~ val(false) := true.
waiting_answer:t+1 ~ val((List:Type)) :=
	action(question(List,Type)).
waiting_answer:t+1 ~ val(false) :=
	action(answer(_,_)).
waiting_answer:t+1 ~ val(B) :=
	waiting_answer:t ~= B.
*/

/*
answer:0 ~ val(none) := true.
answer:t+1 ~ val(FChoice) :=
	action(question(_,choice,FChoice)).
answer:t+1 ~ val(none) :=
	action(answer(_,choice,N)),
	answer:t ~= Choices,
	nth1(N,Choices,Answer). 
answer:t+1 ~ val(Val) :=
	answer:t ~= Val.
*/

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
/*
instances ~ uniform(L2) :=
	findall(X,(instance2(X)),L),
	remove_duplicates(L,L2).
*/
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
	command_content(comm2,Content),
	%writeln(Content),
	nth1(1,Content,Choice).
*/ 
/*
inside_room(human(1)):0 ~ val(kitchen) := true.
inside_room(human(N)):t+1 ~ val(Room) :=
	action(humanloc(human(N),Room)).
inside_room(human(N)):t+1 ~ val(Room) :=
	inside_room(human(N)):t ~= Room.
*/
/*
performing:0 ~ val(none) := true.
performing:t+1 ~ val(Perf) :=
	action(start_perf(Perf)).
performing:t+1 ~ val(none) :=
	performing:t ~= Perf,
	action(success_perf(Perf)).
performing:t+1 ~ val(none) :=
	performing:t ~= Perf,
	action(unsuccess_perf(Perf)).
performing:t+1 ~ val(Perf) :=
	performing:t ~= Perf.
*/


/*
has_position(left_gripper):0 ~ val((0,0,0)) := true.
has_position(left_gripper):t+1 ~ val((X,Y,Z)) :=
	action(pos(left_gripper,[X,Y,Z])).
has_position(left_gripper):t+1 ~ val((X,Y,Z)) :=
	has_position(left_gripper):t ~= (X,Y,Z).

has_position(right_gripper):0 ~ val((0,0,0)) := true.
has_position(right_gripper):t+1 ~ val((X,Y,Z)) :=
	action(pos(right_gripper,[X,Y,Z])).
has_position(right_gripper):t+1 ~ val((X,Y,Z)) :=
	has_position(right_gripper):t ~= (X,Y,Z).
*/
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
	action(command_content(command(N),Content)),
	member(pickup(_),Content).
wants_action(human(N)):t+1 ~ val(true) :=
	human(N) ~= true,
	action(gives_command(human(N),_,Comm)),
	comm_category(Comm):t+1 ~= answerquestioncommands_ca.
wants_action(human(N)):t+1 ~ val(true) :=
	human(N) ~= true,
	action(gives_command(human(N),_,Comm)),
	comm_category(Comm):t+1 ~= performphysicalactioncommands_ca.
*/

/*
action_category(human(N)):0 ~ uniform(L) :=
	human(N) ~= true,
	findall(Sub,subcategory(Sub,performableactions_ca),L).
*/

/*
action_category(human(N)):0 ~ val(none) := true.

action_category(human(N)):t+1 ~ val(pickup_ca) :=
	human(N) ~= true,
	action(gives_command(human(N),_,Comm)),
	action(command_content(Comm,Content)),
	member(pickup(_),Content).
	%has_need(human(N)):t ~= obtain_item.
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

/*
nrobots ~ val(1) := true.
robot(N) ~ val(true) :=
	nrobots ~= NH,
	between(1,NH,N).

ncommands ~ val(1) := true.
command(N) ~ val(true) :=
	ncommands ~= NH,
	between(1,NH,N).
*/

/*
nhumans ~ val(1) := true.
human(N) ~ val(true) :=
	nhumans ~= NH,
	between(1,NH,N).
*/

/*

*/


/*
handling_command:0 ~ val(none) := true.
handling_command:t+1 ~ val(command(N)) :=
	command(N) ~= true,
	action(gives_command(H,R,command(N))).
handling_command:t+1 ~ val(C) :=
	handling_command:t ~= C.
*/


/*
has_category(Cat,object(N)):t+1 ~ val(false) :=
	category(Cat),
	object(N):t ~= true,
	action(segm(object(N),_)).
*/
/*
has_category(Cat,object(N)):t+1 ~ val(true) :=
	category(Cat),
	object(N):t ~= true,
	action(cat_ev(object(N),Cat)),
	writeln(a).
has_category(Super,object(N)):t+1 ~ val(true) :=
	object(N):t ~= true,
	action(cat_ev(object(N),Cat)),
	writeln(t),
	category(Super),
	is_subcategory(Cat,Super).
has_category(Cat,object(N)):t+1 ~ val(false) :=
	object(N):t ~= true,
	action(cat_ev(object(N),_)), 
	writeln(s),	
	category(Cat),
	is_subcategory(Cat,spatialobjects_ca).
*/

/*
has_category(commands_ca,command(N)):0 ~ val(true) :=
	command(N):t ~= true.
*/
/*
has_category(Cat,command(N)):0 ~ val(false) :=
	category(Cat),
	command(N) ~= true.
*/
/*
has_category(Cat,command(N)):t ~ val(true) :=
	comm_category(command(N)):t ~= Cat.
*/

/*
has_position(object(N)):0 ~ finite([1:(0,0,0)]) := 
	object(N):t ~= true.	

has_position(object(N)):t+1 ~ indepGaussians([ ([X],[0.005]), ([Y],[0.005]), ([Z],[0.005]) ]) :=
	observation(object(ID)) ~= (X,Y,Z).	
has_position(object(N)):t+1 ~ finite([1:(X2,Y2,Z2)]) :=
	action(success_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	has_position(Gripper):t ~= (X2,Y2,Z2).
has_position(object(N)):t+1 ~ finite([1:(X2,Y2,Z2)]) :=
	action(success_perf(PName)),
	perf(PName,place,[object(N),X2,Y2,Z2,_]).

has_position(object(N)):t+1 ~ finite([1:(X,Y,Z)]) :=
	has_position(object(N)):t ~= (X,Y,Z).



averageobject(NO,Particles,Mean) :-
	dcpf:bb_get(offset,Offset),
	bb_put(sumobj,0.0),
	(
		between(1,Particles,Pos),
		I is Offset+Pos,
		%writeln(I),
		recorded(I,current(has_position(object(NO))) ~= (X,Y,Z),_),
		writeln(X),
		bb_get(sumobj,OldTOT),

		NewTOT is OldTOT+Val,
		bb_put(sumobj,NewTOT),
		fail;
		true
	),
	bb_delete(sumobj,T),
	Mean is T/Particles.
*/


/*
obsProbability(object(N)):0 ~ finite([1:begin]) := true.
obsProbability(object(N)):t+1 ~ finite(ProbList) := %ProbList
	%writeln(test),	
	obj_category(object(N)):t ~= Cat,
	%writeln(---------------),
	%writeln(cat1:Cat),
	action(obsProb(object(N),L2)),
	%writeln(test),
	%delete(L2, Cat, Rest),
	%append([Cat],Rest,L2),
	%writeln(Rest),	
	getProbList(Cat,L2,0.5,ProbList)
	%writeln(cat2:Cat),
	%writeln(ProbList)
	.

obsProbability(object(N)):t+1 ~ val(Val) :=
	object(N):t ~= true,
	obsProbability(object(N)):t ~= Val.
*/


/*
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

*/

/*
has_colour(object(N)):0 ~ val(unscanned) :=
	object(N):t ~= true.
has_colour(object(N)):t+1 ~ finite(D) :=
	action(col(object(N),D)).
has_colour(object(N)):t+1 ~ val(Col) :=
	has_colour(object(N)):t ~= Col.
*/

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

%gewilde_actie(siegfried):t+1 ~ uniform(ActieLijst) :=
%	uitvoerbaar(pickupacties):t ~= ActieLijst.


/*
has_category(Cat,human(N)):0 ~ finite([1:false]) :=
	category(Cat),
	human(N) ~= true.
*/
/*
has_category(Cat,robot(N)):0 ~ finite([1:false]) :=
	category(Cat),
	robot(N) ~= true.
*/
/*
has_category(Cat,table):0 ~ val(false) :=
	category(Cat).
*/
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

/*
wanted_effect(PName):t+1 ~ val(true) :=
	action(end_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	graspable(object(N),Gripper):t ~=true,
	reachable(object(N),Gripper):t ~= true.
wanted_effect(PName):t+1 ~ val(false) :=
	action(end_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	graspable(object(N),Gripper):t ~=false,
	reachable(object(N),Gripper):t ~= true.
wanted_effect(PName):t+1 ~ val(false) :=
	action(end_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	graspable(object(N),Gripper):t ~=true,
	reachable(object(N),Gripper):t ~= false.
wanted_effect(PName):t+1 ~ val(false) :=
	action(end_perf(PName)),
	perf(PName,pickup,[object(N),_,_,_,Gripper]),
	graspable(object(N),Gripper):t ~=false,
	reachable(object(N),Gripper):t ~= false.
wanted_effect(PName):t+1 ~ val(true) :=
	action(end_perf(PName)).
wanted_effect(PName):t+1 ~ val(true) :=
	action(end_perf(PName)),
	perf(PName,place,[object(N),_,_,_,Gripper]),
	placeable(object(N),Gripper):t ~= true.
wanted_effect(PName):t+1 ~ val(false) :=
	action(end_perf(PName)),
	perf(PName,place,[object(N),_,_,_,Gripper]),

	placeable(object(N),Gripper):t ~= false.




wanted_effect(PName):t+1 ~ val(B) :=

	wanted_effect(PName):t ~= B.

*/

%update met observaties

/*
observation(negfeedback(human(N))):t+1 ~ val(Neg) :=
	wanted_action(human(N)):t+1 ~= WA,
	writeln(WA),
	writeln(Neg),
	WA \= Neg,
	writeln(feedbacktest).
*/

/*
graspable(object(N),right_gripper):0 ~ finite([0.9:true, 0.1:false]) :=
	nobjs:0 ~= Nu,
	between(1,Nu,N). 
graspable(object(N),left_gripper):0 ~ finite([0.9:true, 0.1:false]) :=
	nobjs:0 ~= Nu,
	between(1,Nu,N).
graspable(object(N),GR):t+1 ~ val(B) :=
	graspable(object(N),GR):t ~= B.

placeable(object(N),right_gripper):0 ~ finite([0.9:true, 0.1:false]) :=
	nobjs:0 ~= Nu,
	between(1,Nu,N). 
placeable(object(N),left_gripper):0 ~ finite([0.9:true, 0.1:false]) :=
	nobjs:0 ~= Nu,
	between(1,Nu,N). 
placeable(object(N),Gripper):t+1 ~ val(B) :=
	placeable(object(N),Gripper):t ~= B.
*/

%NP between
%NP cat optimal
%NP number of objects observation model with higher chance of seeing number closer to real number
%,0.1:cleanup need
%TODO definition leftof, etc... taking into account person location
%TODO extend care model with more state knowledge, extend wa with list!
