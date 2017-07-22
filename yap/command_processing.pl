%%% -*- Mode: Prolog; -*-

:- use_module('knowledge_base.pl').
:- use_module('knowledge_processing.pl').
:- use_module('utilities.pl').
:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).
:- dynamic (=:)/2.
:- dynamic has_goal/2.
%:- dynamic expand_goal/2.
builtin(sample_uniform(_,_,_)).
builtin(member(_,_)).

/*
process_command(Comm,T) ~ val(true) :=
	has_category(performphysicalactioncommands_ca,Comm,T) ~= true,
	command_content(Comm,Cont),
	member(pickup(O),Cont),
	find_object_actedon_clues(Cont,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	writeln(tu:Tu),
	evidence(distributionalclause(focus_constraints(O,comm1),finite([1:true]),Tu,1)).
process_command(Comm,T) ~ val(true) := true.	
*/

/*
Main execution loop of a command, with communication back to user.
*/
execute_command(Command, HumanInfo) :-
	nl,
	writeln('Received command from user:'),	
	writeln(Command),
	%%%%HumanInfo = distributionalclause(commandgiver(Cn),finite(Prob),true,1),
	%%%%asserta(HumanInfo),nl,
	
	writeln('Classify clues in command:'),
	classify_clues(Command,Categories),nl,

	writeln('Classify command:'),
	classify_command(Command, Categories, CommandCategory,commandgiver(Cn)),nl,
	
	
	getHuman(HumanInfo, Human),
	write('Human giving command is: '),writeln(Human),nl,
	
	recalculateHumanGoal(Command, Categories, CommandCategory, Human),
	
	has_goal(Human,Goal), %probabilistisch maken in kb
	
	reach_goal(Goal).
	
reach_goal(G) :-
	writeln('Goal is: '),writeln(G),
	%afh van situatie kies welke actie we uitvoeren
	(goal_satisfied(G)->
		writeln('goal satisfied')
	;
		writeln('goal not satisfied in current world'),
		expand_goal2(G,G2),
		get_best_action(G2,Action)
	).
	
	
expand_goal2(G,G2) :-
	nth1(2,G,Clause),%TODO holding
	get_var_w_domain(Clause, time_ca, Var),
	
	timepoint(T),
	T2 is T+1, %TODO neem aan volgende tijdstap
	Var = T2,
	term_variables(G,Vars),
	write('varz'),writeln(Vars),
	asserta(Clause),
	answer_question(G,Answer),
	retract(Clause),
	
	probability_processing(Answer, 0.5, Pruned),
	writeln('pruned'),
	writeln(Pruned),
	get_probs(Pruned, Pr),
	writeln('probs'),
	writeln(Pr),
	max_list(Pr,MaxPr),
	member((MaxPr:Insts),Pruned),
	
	tuple_to_list(Insts,I2),
	writeln(Vars),
	writeln(I2),
	instantiated_goal(G,Vars,I2,InstG),
	write('Instantiated goal is:'),writeln(InstG),
	%%%get_assumptions(G,InstG,InstG),
	
	timepoint(T),
	get_action(T,A),
	asserta(perform(A,T)),
	get_actioncategory(A,InstG,ACat),
	essentialparameters(pickup_ca,Params),
	clear_up(Params,InstG,A)
	. 


%TODO
/*	
get_assumptions(G,IG,Assumps) :-		
*/

instantiated_goal([],Vars,I2,[]).
instantiated_goal([Clause|R],Vars,I2,[Clause2|R2]) :-
	instantiated_goal2(Clause,Vars,I2,Clause2),
	instantiated_goal(R,Vars,I2,R2).
	
instantiated_goal2(distributionalclause(Head,D,Body,Pr),Vars,I2,distributionalclause(Head2,D,Body,Pr)) :-
	functor(Head,Name,N),
	functor(Head2,Name,N),
	instantiated_goal3(Head,Vars,I2,Head2,N).
	
instantiated_goal3(Head,Vars,I2,Head2,0).	
instantiated_goal3(Head,Vars,I2,Head2,N) :-
	arg(N,Head,A1),
	arg(N,Head2,A2),
	(ground(A1) ->
		A2 = A1
	;
		get_index(A1,Vars,Index),
		nth1(Index, I2,Atom),
		A2 = Atom
	),
	N2 is N-1,
	instantiated_goal3(Head,Vars,I2,Head2,N2).
	
get_best_action(Goal,Action) :-
	%spatial config predicates
	
	
	writeln(Goal),
	findall(X,can_perform(pr2_ca,X),L),
	writeln(L). %TODO precondities voldaan in huidige situatie
	
	/*
	timepoint(T),
	get_action(T,A),
	asserta(distributionalclause(actioncategory(A),finite([1:Cat]),true,1)),
	essentialparameters(Cat,Pars),
	writeln('Pars:'),writeln(Pars),
	clear_up(Pars,A),
	%eval_query_particle2(X,perceived(X),100,P),
	writeln(P),
	%append(G2, Pars,G3),
	writeln(G3).
	*/

goal_satisfied(G) :-
	answer_question(G,Answer),
	length(Answer,Length),
	writeln(Length),
	Length > 0.
	
has_goal(H,G) :- false.	
	
recalculateHumanGoal(Command, Categories, explicitactioncommands_ca, Human) :-
	(has_goal(Human,Goal) ->
		append(Goal,Command,Goal2),%TODO beter
		retractall(has_goal(Human,_)),
		asserta(has_goal(Human,Goal2))
	;
		find_action_category(Command,ACat),
		convert_to_goal(ACat, Command, Goal2),
		asserta(has_goal(Human, Goal2))
	).
	
convert_to_goal(pickup_ca, Command, Goal2) :-
	write('command'),
	writeln(Command),
	append(Command,[distributionalclause(holding_object(Arm,O,T),finite([1:true]),true,1)],Goal),
	%append(Command,[],Goal),
	writeln(Goal),
	member(distributionalclause(pickup(O),finite([1:true]),true,1),Goal),
	delete(Goal,(distributionalclause(pickup(O),finite([1:true]),true,1)),Goal2).

/*
Answer question command, query database.
*/
execute_command2(Command, Categories, answerquestioncommands_ca, Cn) :-
	answer_question(Command,Answer).

/*
Explicit action mentioned in command.
Find missing action parameters to perform action.
*/
execute_command2(Command, Categories, explicitactioncommands_ca,Cn) :-
	find_missing_info(Command, CommandCategory, MissingInfo),	
	writeln('Missing information to perform action: '), writeln(MissingInfo),nl,	
	!,

	writeln('Clearing up missing information.'),
	clear_up(MissingInfo,Command),nl,

	writeln('Performing action.'),
	perform_current_action,
	writeln('Action performed').

/*
Command with wanted effect info and object info => infer action to be performed.
*/
execute_command2(Command, Categories, performphysicalactioncommands_ca,Cn) :-
	has_effectinfo(Categories),
	has_objectinfo(Categories),
	writeln('Command contains effect and object info, infer action.'),
	%TODO geef voorlopig alles mee, vervangen door effect eruithalen, object variabelen er uit halen (maak predicaat) en uiteindelijk de resulterende clues meegeven. vb'en voorlopig op maat van deze procedure
	%TODO get object clues maken
	member(distributionalclause(near(O1,O2,T),finite([1:true]),true,_),Command),%TODO ook alternatief voor niet near
	reach_goal(near(O1,O2),Command).

/*
Command with only object info => deduce both action and effects.
*/
execute_command2(Command, Categories, performphysicalactioncommands_ca,Cn) :-
	has_objectinfo(Categories),
	writeln('Command contains only object info, infer action with end goal'),
	distributionalclause:eval_query_backward_distrib([],[],(A),(commandgiver(Cn)~=H, objectaction(H) ~= A),1000,P),
	changelist(P,P2),
	keysort(P2,P3),
	reverse(P3,P4),
	write('Predicted actions: '),writeln(P4),
	probability_processing(P4, 0.5, Return),
	get_values(Return, Values),
	length(Values,Length),
	( Length > 1 ->
		writeln('Too uncertain about action, asking user.'),
		ask_actiontype(Values,ChosenA)
	;
		writeln('Action clear enough based on probabilities.'),
		nth1(1,Values,ChosenA)
	),
	%get_best_result(P4,ChosenComm)
	write('Chosen action: '),writeln(ChosenA).
	%TODO extend command and try to perform again!!!.

/*
Command with only effect info => deduce both action and objects.
*/
execute_command2(Command, Categories, performphysicalactioncommands_ca,Cn) :-
	has_effectinfo(Categories),
	writeln('Command contains only effect info, infer action on object(s).'),
	member(distributionalclause(near(O1,O2,T),finite([1:true]),true,_),Command),%TODO ook alternatief voor niet near
	reach_goal(near(O1,O2),[]).
	

has_objectinfo(CommandCategories) :-
	findall(X, is_subcategory(X,objectclues_ca), L),
	findall(Y, (member(Y,L), member(Y,CommandCategories)),L2),
	L2 \= [].
	%writeln(L2).

has_effectinfo(CommandCategories) :-
	findall(X, is_subcategory(X,effectclues_ca), L),
	findall(Y, (member(Y,L), member(Y,CommandCategories)),L2),
	L2 \= [].
	%writeln(L2).

/*
Classification of clues in command.
*/
classify_clues([],[]).
classify_clues([Clue|Rest],[ClueCategory|R]) :-
	write('Clue: '),writeln(Clue),	
	classify_clue(Clue, ClueCategory),
	write('Has category: '),writeln(ClueCategory),
	classify_clues(Rest,R).
classify_clue(distributionalclause(actioncategory(I),finite([P:X]),_,_), ClueCategory) :-
	subcategory(X, physicalactions_ca),%%%todo is_subcat
	ClueCategory = explicitactionmentions_ca.
classify_clue(distributionalclause(pickup(_),_,_,_), ClueCategory) :-
	%%pickup koppelen aan klasse, etc..
	ClueCategory = explicitactionmentions_ca.
classify_clue(distributionalclause(current(has_category(X,I)),finite([1:true]),_,_), ClueCategory) :-
	is_subcategory(X, spatialobjects_ca),
	ClueCategory = objectcategorydefinitions_ca.
classify_clue(distributionalclause(current(has_colour(O)),finite([P:X]),true,1), ClueCategory) :-
	ClueCategory = objectpropertydefinitions_ca.
classify_clue(distributionalclause(acts_on_object(A),finite([1:O]),true,1), ClueCategory) :-
	ClueCategory = objectactedondescriptions_ca.
classify_clue(distributionalclause(has_performer(A),finite([P:Perf]),true,1), ClueCategory) :-
	ClueCategory = performerdescriptions_ca.
classify_clue(distributionalclause(has_category(Performer),finite([P:PerformerCategory]),_,_), ClueCategory) :-
	is_subcategory(PerformerCategory, actionperformers_ca),	
	ClueCategory = parameterdescriptions_ca.

classify_clue(distributionalclause(current(near(_,_)),finite([P:B]),_,_), ClueCategory) :-	
	ClueCategory = spatialclues_ca.
classify_clue(distributionalclause(current(leftof(_,_)),finite([P:B]),_,_), ClueCategory) :-	
	ClueCategory = spatialclues_ca.
classify_clue(distributionalclause(current(rightof(_,_)),finite([P:B]),_,_), ClueCategory) :-	
	ClueCategory = spatialclues_ca.

/*
Classify command.
*/
classify_command(Command, Categories, CommandCategory, commandgiver(Cn)) :-
	member(explicitactionmentions_ca, Categories),
	CommandCategory = explicitactioncommands_ca,
	write('Chosen command category: '), writeln(CommandCategory).
classify_command(Command, Categories, ChosenComm, commandgiver(Cn)) :-
	%bij resultaat maken we abstractie ovr humans heen, enkel type command is belangrijk
	writeln('Calculating command category chances based on human recognition chances and individual models.'),
	distributionalclause:eval_query_backward_distrib([],[],(ComCat),(commandgiver(Cn)~=H, commandcat(H) ~= ComCat),1000,P),
	changelist(P,P2),
	keysort(P2,P3),
	reverse(P3,P4),
	write('Predicted categories with chances: '),writeln(P4),
	probability_processing(P4, 0.5, Return),
	get_values(Return, Values),
	length(Values,Length),
	( Length > 1 ->
		writeln('No clear enough command category, asking human.'),
		ask_commandtype(Values,ChosenComm)
	;
		writeln('Clear enough command category based on probabilities.'),
		nth1(1,Values,ChosenComm)
	),
	%get_best_result(P4,ChosenComm)
	write('Chosen command category: '), writeln(ChosenComm).


	

	%TODO in kb zetten

/*
Clearing up missing action parameters.
*/
clear_up([],_,_) :-
	writeln('Cleared up missing param info.').
clear_up([object_pickedup(pickup_ca, moveablethings_ca)|R],Goal,Action) :-
	writeln('Clear up object acted on.'),
	find_object_picked_up(Goal, Action, Distribution),
	
	changelist(Distribution,R2),
	keysort(R2,R3),
	reverse(R3,R4),
	write('Chances of objects being object being acted on: '),writeln(R4),
	get_highest_chances(R4,HC), %TODO vervangen met nieuwe methode
	length(HC,Length),
	( Length==1 ->
		writeln('Certain enough about object.'),
		member(Obj,HC)
	;
		%multiple objects, ask user
		writeln('Too uncertain, finding object differences and asking human.'),
		find_object_differences(Cat, HC, Diffs),
		ask_object_actedon(Cat,pickup_ca,HC,Diffs,Obj)
	),
	write('Action will act on: '),writeln(Obj),	
	timepoint(Time),
	get_action(Time,A),
	asserta(distributionalclause(object_pickedup(A),finite([1:Obj]),true,1)),
	clear_up(R,Goal,Action).

clear_up([has_performer(pickup_ca, actionperformers_ca)|R],Goal,Action) :-
	timepoint(Time),
	get_action(Time,A),
	asserta(distributionalclause(has_performer(A),finite([1:pr2robot1]),true,1)), %%%
	write('Performer of action is: '),writeln(pr2robot1),	
	clear_up(R,Goal,Action).
	
clear_up([uses_gripper(pickup_ca, robotarms_ca)|R],Goal,Action) :-
	writeln('Clear up gripper used.'),
	find_gripper_used(Goal, Action, Distribution),
	
	changelist(Distribution,R2),
	keysort(R2,R3),
	reverse(R3,R4),
	write('Chances of grippers being used: '),writeln(R4),
	get_highest_chances(R4,HC), %TODO vervangen met nieuwe methode
	length(HC,Length),
	( Length==1 ->
		writeln('Certain enough about gripper.'),
		member(Obj,HC)
	;
		%multiple objects, ask user
		writeln('Too uncertain, ask human.'),
		%find_object_differences(Cat, HC, Diffs),
		ask_object_actedon(Cat,pickup_ca,HC,Diffs,Obj) %TODO vervangen
	),
	write('Action will use gripper: '),writeln(Obj),	
	timepoint(Time),
	get_action(Time,A),
	asserta(distributionalclause(uses_gripper(A),finite([1:Obj]),true,1)).
	clear_up(R,Goal,Action).

find_action_category(Command, ActionCategory) :-
	is_subcategory(ACat, actions_ca),
	member(distributionalclause(actioncategory(A),finite([P:ACat]),true,1),Command),
	ActionCategory = ACat,
	write('Action category is: '),writeln(ACat).
%%beter
find_action_category(Command, ActionCategory) :-
	is_subcategory(ACat, actions_ca),
	member(distributionalclause(pickup(O),_,_,_),Command),
	ActionCategory = pickup_ca,
	write('Action category is: '),writeln(pickup_ca).

get_action_clue([],[]).
get_action_clue([H|R],AC) :-
	classify_clue(H,explicitactionmentions_ca),
	AC = H.
get_action_clue([H|R],AC) :-
	get_action_clue(R,AC). 

perform(Command) :-
	has_category(Command) = commands_ca,
	causes(Command, Action),
	perform(Action).

changelist([],[]).
changelist([(H1:H2)|R1],[(H1-H2)|R2]) :-
	changelist(R1,R2).

changelist2([],[]).
changelist2([(H1,H2)|R1],[(H1-H2)|R2]) :-
	changelist2(R1,R2).

changelist3([],[]).
changelist3([(H1-H2)|R1],[(H1:H2)|R2]) :-
	changelist3(R1,R2).

get_best_result([H|R],Result) :-
	H = (_-Result).


%(Prob-X) elements, ordered list (high to low)
get_highest_chances(L, L2) :-
	L = [(Prob-Y)|R],
	findall(X, member((Prob-X),L),L2).

find_performerclues([],_,[]).
find_performerclues([Clue|R],P,[Clue|R2]) :-
	term_variables(Clue,Vars),	
	member(P,Vars),
	classify_clue(Clue,Cat),
	Cat = performerdescriptions_ca,
	find_performerclues(R, P, R2).

/*
find_performerclues([Clue|R],P,[Clue|R2]) :-
	classify_clue(Clue,Cat),
	Cat = explicitactionmentions_ca,
	find_performerclues(R, P, R2).
*/

find_performerclues([Clue|R], P, R2) :-
	find_performerclues(R, P, R2).

find_object_actedon_clues([],_,[]).
find_object_actedon_clues([Clue|R],O,[Clue|R2]) :-
	%writeln(uuu),
	term_variables(Clue,Vars),
	%writeln(Vars),	
	%memberchk(O,Vars),%%TODO
	classify_clue(Clue,Cat),
	%writeln(Clue),
	(Cat = objectpropertydefinitions_ca ;Cat = objectcategorydefinitions_ca; Cat = explicitactionmentions_ca;Cat = spatialclues_ca),%;Cat = objectactedondescriptions_ca
	find_object_actedon_clues(R, O, R2).
find_object_actedon_clues([Clue|R],O,R2) :-
	find_object_actedon_clues(R, O, R2).

/*
Add new command-evidence to local knowledge base.
*/
add_new_command_evidence([]).
add_new_command_evidence([H|R]) :-
	asserta(H),
	writeln(added_to_command_kb:H), 
	add_new_command_evidence(R).

/*
Remove command-evidence from local knowledge base.
*/
remove_command_evidence([]).
remove_command_evidence([H|R]) :-
	retract(H),
	writeln(removed_from_command_kb:H), 
	remove_command_evidence(R).

%TODO near uit command halen anders heel veel backtrack!!!!! voorlopig command volgorde aangepast
reach_goal(near(O1,O2),Command):-
	writeln(--------),
	writeln(reach_near_goal),
	timepoint(T),
	writeln(time: T),
	T2 is T+1,

	convert_to_query(Command,Q3),
	writeln(extra_info:Q3),
	list_to_tuple(Q3,Tuple),
	get_instances(moveablethings_ca, Objs),

	findall(P2, 
	(
	
	member(O1, Objs),
	member(O2, Objs),
	O1\=O2,
	asserta(distributionalclause(near(O1,O2,T2),finite([1:true]),true,1)),
 	distributionalclause:eval_query_backward_distrib(
		[],
		[],
		(O1,O2),
		(Tuple)
	,200,P2
	),
	retract(distributionalclause(near(O1,O2,T2),finite([1:true]),true,1)), P2\=[]
	)
	,R2),
	flatten(R2,R22),
	writeln(R22),

	member((_:(O1,O2)),R22),
	asserta(distributionalclause(near(O1,O2,T2),finite([1:true]),true,1)),
	asserta(distributionalclause(near(O2,O1,T2),finite([1:true]),true,1)),%TODO

	term_variables(Command,Vars),
	list_to_tuple(Vars,Vars2),
	%maak effecten klasse met instances
	%asserta(distributionalclause(near(O1,O2,T2),finite([1:true]),true,1)), %TODO moet nu beneden staan, O1 gaat bij near 3 over alle moveable objects
	get_instances(graspablethings_ca, Objs2),
	findall(P, 
	(
	member(O,Objs2),
	%asserta(distributionalclause(acts_on_object(action0),finite([1:O]),true,1)),%TODO action0
	distributionalclause:eval_query_backward_distrib(
		[],
		[],
		(Cat,O,O1,O2),
		(has_category(O) ~=Ocat,is_subcategory(Ocat,moveablethings_ca),actioncategory(action0) ~= Cat, has_effect(action0,O,T) ~= movedsomething(O,T),movedsomething(O,T)~=true)
	,200,P
	)
	%retract(distributionalclause(acts_on_object(action0),finite([1:O]),true,1))
	)
	,R),
	writeln(predicted_actions),
	flatten(R,RR),
	writeln(RR).


