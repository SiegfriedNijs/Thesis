%%% -*- Mode: Prolog; -*-
:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module('knowledge_base.pl').
:- use_module('knowledge_processing.pl').
:- use_module('command_processing.pl').
:- use_module('perception_module.pl').
:- use_module('communication_module.pl').

certain_subcategorytest :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),
	certain_lowest_subcategories(object(1),L1),
	writeln(L1),
	certain_lowest_subcategories(object(2),L2),
	writeln(L2),
	certain_lowest_subcategories(object(3),L3),
	writeln(L3).

unique_descriptionstest :-
	N is 200,
	init_particle(N),
	get_couples([object(1),object(2),object(3)],Couples),
	writeln(couples: Couples),
	cupsworld3re(N),
	writeln(test1),
	unique_object_descriptions([object(1),object(2),object(3)],D),
	writeln(D),
	writeln(test2),
	unique_object_descriptions([object(3),object(1)],D1),
	writeln(D1).

evidence_test :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),
	obj_current_query(object(1),N),
	unique_object_descriptions([object(1),object(2),object(3)],D),
	writeln(D),
	nl,writeln('User gives evidence that object(1) is cup.'),nl,
	step_particle([action(cat_ev(object(1),cups_ca))],[],N),
	obj_current_query(object(1),N),
	unique_object_descriptions([object(1),object(2),object(3)],D1),
	writeln(D1).

ask_focustest :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),
	ask_focus(moveablethings_ca,pickup_ca,[object(1),object(2),object(3)]).

common_categorytest :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),
	writeln('Finding certain common lowest category for object 1 to 3, should be moveablething.'),
	get_common_category([object(1),object(2),object(3)],Common),
	write('Common lowest category: '),writeln(Common).
	
near_test_notime :-
	init,
	cupsworld3,	
	
	distributionalclause:eval_query_backward_distrib( [] , [] ,(Near) , (near(object1,object2,0)~=Near) , 1 , P1),
	writeln(P1), %should be true

	distributionalclause:eval_query_backward_distrib( [] , [] ,(Near1) , (near(object2,object1,0)~=Near1) , 1 , P2),
	writeln(P2), %should be true

	distributionalclause:eval_query_backward_distrib( [] , [] ,(Near2) , (near(object1,object3,0)~=Near2) , 1 , P3),
	writeln(P3), %should be false

	distributionalclause:eval_query_backward_distrib( [] , [] ,(Near3) , (near(object2,object3,0)~=Near3) , 1 , P4),
	writeln(P4), %should be false

	distributionalclause:eval_query_backward_distrib( [] , [] ,(Near4) , (near(object2,object2,0)~=Near4) , 1 , P5),
	writeln(P5). %should be true


on_test_notime :-
	init,
	cupsworld3,
	
	distributionalclause:eval_query_backward_distrib( [] , [] ,(On1) , (on(object1,object2,0)~=On1) , 1000 , P1),
	writeln(P1), %should be false

	distributionalclause:eval_query_backward_distrib( [] , [] ,(On2) , (on(object2,table1,0)~=On2) , 1000 , P2),
	writeln(P2), %should be true	

	distributionalclause:eval_query_backward_distrib( [] , [] ,(On3) , (on(object1,object1,0)~=On3) , 1000 , P3),
	writeln(P3), %should be false

	distributionalclause:eval_query_backward_distrib( [] , [] ,(On4) , (on(table1,object3,0)~=On4) , 1000 , P4),
	writeln(P4). %should be false

leftof_test_notime :-
	init,
	cupsworld3,
	
	distributionalclause:eval_query_backward_distrib( [] , [] ,(L1) , (leftof(object1,object1,0)~=L1) , 1000 , P1),
	writeln(P1), %should be false

	distributionalclause:eval_query_backward_distrib( [] , [] ,(L2) , (leftof(object1,object2,0)~=L2) , 1000 , P2),
	writeln(P2), %should be true	

	distributionalclause:eval_query_backward_distrib( [] , [] ,(L3) , (leftof(object1,table1,0)~=L3) , 1000 , P3),
	writeln(P3), %should be false

	distributionalclause:eval_query_backward_distrib( [] , [] ,(L4) , (leftof(object3,object1,0)~=L4) , 1000 , P4),
	writeln(P4). %should be false

rightof_test_notime :-
	init,
	cupsworld3,
	
	distributionalclause:eval_query_backward_distrib( [] , [] ,(R1) , (rightof(object1,object1,0)~=R1) , 1000 , P1),
	writeln(P1), %should be false

	distributionalclause:eval_query_backward_distrib( [] , [] ,(R22) , (rightof(object2,object1,0)~=R22) , 1000 , P22),
	writeln(P22), %should be false	

	distributionalclause:eval_query_backward_distrib( [] , [] ,(R2) , (rightof(object1,object2,0)~=R2) , 1000 , P2),
	writeln(P2), %should be false	

	distributionalclause:eval_query_backward_distrib( [] , [] ,(R3) , (rightof(object1,table1,0)~=R3 ), 1000 , P3),
	writeln(P3), %should be false

	distributionalclause:eval_query_backward_distrib( [] , [] ,(R4) , (rightof(object3,object1,0)~=R4) , 1000 , P4),
	writeln(P4). %should be true


%should be false
interval_test1 :-
	overlapping([0,4],[5,8]).

%should be true
interval_test2 :-
	overlapping([2,6],[5,8]).

%should be true
interval_test3 :-
	overlapping([2,6],[4,8]).

%should be false
interval_test33 :-
	overlapping([2,6],[7,8]).

%should be 0
interval_test4 :-
	distance_interval([2,6],[5,8],D),
	writeln(D).

%should be 1
interval_test5 :-
	distance_interval([2,6],[7,8],D),
	writeln(D).

%should be 4
interval_test6 :-
	distance_interval([12,16],[7,8],D),
	writeln(D).




