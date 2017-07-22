%%% -*- Mode: Prolog; -*-
:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module('knowledge_base.pl').
:- use_module('command_processing.pl').

cupsworld2 :-
	evidence(distributionalclause( obj_category(object1) ,finite([0.81:cups_ca,0.19:sodacans_ca]) ,true,1)),
	evidence(distributionalclause( obj_category(object2) ,finite([0.6:cups_ca,0.4:sodacans_ca]) ,true,1)),
	evidence(distributionalclause( has_position(object1, 0) ,val((10,15,54)),true,1)),
	evidence(distributionalclause( has_position(object2, 0) ,val((20,15,54)),true,1)),

	evidence(has_dimensions(object1, [4,4,8])),
	evidence(has_dimensions(object2, [4,4,8])),

	evidence(distributionalclause(has_colour(object1), finite([1:red]),true,1)),
	evidence(distributionalclause(has_colour(object2), finite([1:white]),true,1)),

	evidence(distributionalclause( has_category(humans_ca, human1,_),finite([1:true]),true,1)),
	evidence(distributionalclause( has_category(pr2_ca,pr2robot1,_),finite([1:true]),true,1)),
	evidence(myself(pr2robot1)),

	evidence(distributionalclause( has_category(tables_ca,table1,_),finite([1:true]),true,1)),
	evidence(workingplace(table1)),
	evidence(distributionalclause( has_position(table1,0) ,val((50,25,25)),true,1)).


cupsworld3 :-

	add_evidence(distributionalclause( has_category(spatialobjects_ca,object1,0), finite([1:true]),true,1)),%
	add_evidence(distributionalclause( has_category(spatialobjects_ca,object2,0), finite([1:true]),true,1)),%
	add_evidence(distributionalclause( has_category(spatialobjects_ca,object3,0), finite([1:true]),true,1)),%

	add_evidence(distributionalclause(has_colour(object1), finite([1:red]),true,1)),%
	add_evidence(distributionalclause(has_colour(object2), finite([1:white]),true,1)),%
	add_evidence(distributionalclause(has_colour(object3), finite([1:red]),true,1)),%

	add_evidence(distributionalclause( obj_category(object1) ,finite([0.81:cups_ca,0.19:sodacans_ca]) ,true,1)), %
	add_evidence(distributionalclause( obj_category(object2) ,finite([1:sodacans_ca]) ,true,1)), %
	add_evidence(distributionalclause( obj_category(object3) ,finite([0.99:cups_ca,0.01:sodacans_ca]) ,true,1)), %	

	add_evidence(distributionalclause( has_position(object1, 0) ,finite([1:(10,15,54)]),true,1)),%
	add_evidence(distributionalclause( has_position(object2, 0) ,finite([1:(18,15,54)]),true,1)),%
	add_evidence(distributionalclause( has_position(object3, 0) ,finite([1:(40,15,54)]),true,1)),%
	
	add_evidence(has_dimensions(object1, [4,4,8])),%
	add_evidence(has_dimensions(object2, [4,4,8])),%
	add_evidence(has_dimensions(object3, [4,4,8])),%
	add_evidence(has_dimensions(table1, [100,50,50])),%
	
	add_evidence(instance2(object1)),%wsl niet nodig
	add_evidence(instance2(object2)),%
	add_evidence(instance2(object3)),%
	add_evidence(instance2(object1content)),%
	add_evidence(instance2(object2content)),%
	add_evidence(instance2(object3content)),%

	add_evidence(holds_content(object1,object1content)),
	add_evidence(holds_content(object2,object2content)),
	add_evidence(holds_content(object3,object3content)),

	add_evidence(distributionalclause(holds_content(object1,object1content,0), finite([1:true]),true,1)),
	add_evidence(distributionalclause(holds_content(object2,object2content,0), finite([1:true]),true,1)),
	add_evidence(distributionalclause(holds_content(object3,object3content,0), finite([1:true]),true,1)),

	add_evidence(perceived(object1)),%wsl niet nodig
	add_evidence(perceived(object2)),%
	add_evidence(perceived(object3)),%

	add_evidence(distributionalclause( has_category(humans_ca, human1,0),finite([1:true]),true,1)),%
	add_evidence(distributionalclause( has_category(pr2_ca,pr2robot1,_),finite([1:true]),true,1)),%
	add_evidence(myself(pr2robot1)),%

	add_evidence(distributionalclause( has_category(tables_ca,table1,_),finite([1:true]),true,1)),%
	add_evidence(workingplace(table1)),%niet nodig?
	add_evidence(distributionalclause( has_position(table1,0) ,finite([1:(50,25,25)]),true,1)).%

cupsworld3re(N) :-

	step_particle([action(col(object(1),red)),action(col(object(2),white)),action(col(object(3),red))],[],N)
	
	.

/*
has_category(object1) ~ finite([0.81:cups_ca,0.19:sodacans_ca]) := true.
has_colour(object1) ~ finite([0.7:black,0.3:red]) := true.
has_category(object2) ~ finite([0.6:sodacans_ca, 0.4:cups_ca]) := true.
has_colour(object2) ~ finite([0.7:black,0.3:red]) := true.
has_category(object3) ~ finite([0.8:cups_ca,0.2:sodacans_ca]) := true.
has_colour(object3) ~ finite([1:blue]) := true.
has_category(object4) ~ finite([0.7:cups_ca,0.3:sodacans_ca]) := true.
has_colour(object4) ~ finite([1:red]) := true.
has_category(object5) ~ finite([1:cups_ca]) := true.
has_colour(object5) ~ finite([1:white]) := true.

has_category(left_arm) ~ finite([1:robotarms_ca]) := true.
has_category(right_arm) ~ finite([1:robotarms_ca]) := true.

has_position(object1, 0) ~ finite([1:(25,15,54)]) := true.
has_position(object2, 0) ~ finite([1:(50,15,54)]) := true.
has_position(object3, 0) ~ finite([1:(75,15,56)]) := true.
has_position(object4, 0) ~ finite([1:(50,40,56)]) := true.
has_position(object5, 0) ~ finite([1:(-20,-20,-20)]) := true. %TODO
has_position(table1,  0) ~ finite([1:(50,25,25)]) := true.

has_dimensions(object1, [4,4,8]).
has_dimensions(object2, [4,4,8]).
has_dimensions(object3, [5,5,12]).
has_dimensions(object4, [5,5,12]).
has_dimensions(object5, [5,5,12]).
has_dimensions(table1, [100,50,50]).

%%%%holding_object(left_arm, object5, 0) ~ finite([1:true]) := true.
holding_object(left_arm, O, 0) ~ finite([1:false]) := true.
holding_object(right_arm, O, 0) ~ finite([1:false]) := true.

%real world
instance2(pr2robot1).
instance2(human1).
instance2(table1).
instance2(object1). %soda can, empty
instance2(object2). %soda can, full of cola
instance2(object3). %blue cup, full of water
instance2(object4). %red cup, full of cola
instance2(object5). %empty cup
instance2(object1content). %empty
instance2(object2content). %cola
instance2(object3content). %water
instance2(object4content). %cola
instance2(object5content). %empty
instance2(left_arm).
instance2(right_arm).


instance2(red).
instance2(green).
instance2(yellow).
instance2(blue).
instance2(black).
instance2(white).

%perceived by robot
perceived(object1).
perceived(object2).
perceived(object3).	
perceived(object4).	
perceived(object5).
perceived(table1).

*/
