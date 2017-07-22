%%% -*- Mode: Prolog; -*-
:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module('knowledge_base.pl').
:- use_module('command_processing.pl').
:- use_module('perception_module.pl').
%:- expects_dialect(sicstus).
:- set_magic(backward). 

lifted(false).
raoblackwellisation(false).

s :-
	N is 300,
	initdcpf(N),
	nobjsObservationInit(3,N),
	typeObservationInit(1,[0.5:cups_ca,0.5:sodacans_ca],N),
	typeObservationInit(2,[1:sodacans_ca],N),
	typeObservationInit(3,[1:sodacans_ca],N),
	locationObservationInit(1,[4,4,8],[10,15,54],N),
	locationObservationInit(2,[4,4,8],[18,15,54],N),
	locationObservationInit(3,[4,4,8],[40,15,54],N),
	tableObservation([100,50,50],[50,25,25],N),
	actionDemandObservation(human(1),N),

	Command = [current(wanted_action(human(1))) ~= (AT,F,left_gripper),current(has_category(cups_ca,F))~=true,current(near(F2,F))~=true,current(has_category(sodacans_ca,F2))~=true],
	commandObservation(human(1),Command,N),
	obj_current_query(object(1),N).
		
		
	%eval_query_particle2(O,current(has_category(cups_ca,O))~=true, N, R2),

	/*
	(
		between(1,N,I),
		abolish_all_tables,
		eraseall(tempparticle),
		%findall(W:X,(proof_query_backward_exp_eval(I,[],NV2,Query,W)),List)
		findall(1:O,(distributionalclause:proof_query_backward(I,tempparticle,current(has_category(cups_ca,object(O)))~=true)),List),
		writeln(List),
		proof_query_backward(I,current(nobjs)~=B),
		writeln(particle:I),		
		writeln(nobjs:B),
		init_query(I,current(has_category(cups_ca,object(2)))~=true),
		%writeln(test),
		proof_query_backward(I,current(has_category(cups_ca,object(O)))~=true),
		%proof_query_backward(I,current(has_category(cups_ca,object(2)))~=true),
		writeln(ok),
		fail,writeln(fail);
		true,writeln(true)
	).
	*/


check_particles2(N) :-
	(
		between(1,N,I),
		writeln(' '),
		write('Particle: '), writeln(I),
		printp(I),
		fail;
		true
	).

initdcpf(N) :-
	writeln('Initializing dcpf.'),
	init_particle(N),
	writeln('Dcpf ready.').

nobjsObservationInit(NO,N) :-
	nobjs_current_query(N),
	step_particle([observation(nobjs)~=NO],[],N),
	nobjs_current_query(N).

typeObservationInit(Obj,Cats,N) :-
	nl,
	write('New first type observation for obj '),write(Obj),nl,
	writeln('Initial object status'),
	writeln(-----------------------),
	obj_current_query(object(Obj),N),
	step_particle([action(segm(object(Obj),Cats))],[],N),
	writeln('Final object status'),
	writeln(-----------------------),
	obj_current_query(object(Obj),N).
	
locationObservationInit(Obj,ObjDim,ObjLoc,N) :-
	nl,	
	write('New first place observation for obj '),write(Obj),nl,
	writeln('Initial object status'),
	writeln(-----------------------),
	obj_current_query(object(Obj),N),
	step_particle([action(pos(object(Obj),ObjLoc))],[],N),
	step_particle([action(dim(object(Obj),ObjDim))],[],N),
	writeln('Final object status'),
	writeln(-----------------------),
	obj_current_query(object(Obj),N).

tableObservation(TableDim,TablePos,N) :-
	nl,	
	writeln('Table observation'),
	writeln(-----------------------),
	step_particle([action(dim(table,TableDim))],[],N),
	step_particle([action(pos(table,TablePos))],[],N),
	table_current_query(table,N).	

actionDemandObservation(Human,N) :-
	nl,	
	writeln('Human indicates action wanted, can be following.'),
	writeln(-----------------------),
	step_particle([action(wabegin)],[],N),
	eval_query_particle2((H,B2),current(wanted_action(H))~=B2,N,R2),
	writeln(R2).

commandObservation(Human,Q,N) :-
	list_to_tuple(Q,Q2),
	%eval_query_particle2((H,B2),current(wanted_action(H))~=B2,N,R2),
	get_variables(Q,L2),
	flatten(L2,L3),
	remove_duplicates(L3,L4),
	%writeln(L4),
	list_to_tuple(L4,L5),
	eval_query_particle3(L5,Q2,N,P),
	step_particle3,
	nl,	
	writeln('Human gave command, new action estimation.'),
	writeln(-----------------------),
	eval_query_particle2((H,B2),current(wanted_action(H))~=B2,N,R2),
	writeln(R2).

scenario11 :-
	N is 1,
	
	init_particle(N),
	eval_query_particle2(B,current(has_category(humans_ca,human1))~=B,N,R),
	writeln(R),nl,

	step_particle([action(human)],[],N),	
	eval_query_particle2(B1,current(has_category(humans_ca,human1))~=B1,N,R1),
	writeln(R1),nl,

	step_particle([action(nonhuman)],[],N),	
	eval_query_particle2(B2,current(has_category(humans_ca,human1))~=B2,N,R2),
	writeln(R2),nl.

s1 :-
	N is 200,
	init_particle(N),

	human_current_query(N),
	robot_current_query(N),
	table_current_query(table,N),
	obj_current_query(object(3),N),
	command_current_query(command(1),N),
	
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),
	cupsworld3re(N),

	
	human_current_query(N),
	wanted_actioninfo(human(1),N),
	robot_current_query(N),
	table_current_query(table,N),
	obj_current_query(object(3),N),
	command_current_query(command(1),N),
	need_current_query(human(1),N),

	/*
	find_object_actedon_clues([pickup(O)],O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	*/

	add_evidence(distributionalclause(focus_constraints(O,command(1)),finite([1:true]),true,1)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),[pickup(O)]))],[],N),

	obj_current_query(object(3),N),
	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N).

nobjs_current_query(N) :-
	eval_query_particle2(B,current(nobjs)~=B, N, R2),
	write('Number of objects:'), writeln(R2),nl.

obj_current_query(object(NO),N) :-
	eval_query_particle2(B,current(object(NO))~=B, N, R2),
	write('Object exists:'), writeln(R2),

	eval_query_particle2(C,current(obj_category(object(NO)))~=C, N, R1),
	write('Object category:'), writeln(R1),

	eval_query_particle2(C11,current(has_position(object(NO)))~=C11, N, R11),
	write('Object position:'), writeln(R11),

	eval_query_particle2(Cd,current(has_dimensions(object(NO)))~=Cd, N, Rd),
	write('Object dimensions:'), writeln(Rd),

	eval_query_particle2(C111,current(has_colour(object(NO)))~=C111, N, R111),
	write('Object colour:'), writeln(R111).

	/*
	eval_query_particle2(C1,current(has_category(moveablethings_ca,object(NO)))~=C1, N, R3),
	write('Is moveable thing:'), writeln(R3),

	eval_query_particle2(C2,current(has_category(cups_ca,object(NO)))~=C2, N, R4),
	write('Is cup:'), writeln(R4),

	eval_query_particle2(C3,current(has_category(sodacans_ca,object(NO)))~=C3, N, R5),
	write('Is soda can:'), writeln(R5),

	eval_query_particle2(C6,current(has_category(robots_ca,object(NO)))~=C6, N, R6),
	write('Is robot:'), writeln(R6),nl.
	*/

table_current_query(table,N) :-
	eval_query_particle2(C11,current(has_position(table))~=C11, N, R11),
	write('Object position:'), writeln(R11),

	eval_query_particle2(Cd,current(has_dimensions(table))~=Cd, N, Rd),
	write('Object dimensions:'), writeln(Rd).

	/*
	eval_query_particle2(C2,current(has_category(tables_ca,table))~=C2, N, R4),
	write('Is table:'), writeln(R4),

	eval_query_particle2(C3,current(has_category(sodacans_ca,table))~=C3, N, R5),
	write('Is soda can:'), writeln(R5),

	eval_query_particle2(C6,current(has_category(anything_ca,table))~=C6, N, R6),
	write('Is anything:'), writeln(R6),nl.
	*/

human_current_query(N) :-
	distributionalclause:eval_query_backward_distrib([],[],NH,nhumans~=NH,N,D),
	write('Number humans:'), writeln(D),
	
	eval_query_particle2(C6,current(has_category(humans_ca,human(1)))~=C6, N, R6),
	write('Is human:'), writeln(R6),
	eval_query_particle2(C2,current(has_category(actionperformers_ca,human(1)))~=C2, N, R2),
	write('Is actionperf:'), writeln(R2),
	eval_query_particle2(Ca,current(has_category(anything_ca,human(1)))~=Ca, N, Ra),
	write('Is anything:'), writeln(Ra),
	eval_query_particle2(C3,current(has_category(robots_ca,human(1)))~=C3, N, R3),
	write('Is robot:'), writeln(R3),
	eval_query_particle2(C1,current(has_category(cups_ca,human(1)))~=C1, N, R1),
	write('Is cup:'), writeln(R1),nl.

performance_current_query(N) :-
	eval_query_particle2(P11,current(performing)~=P11, N, P1),
	write('Performing:'), writeln(P1),
	eval_query_particle2((Perf,C),current(good_perform(Perf))~=C, N, R),
	write('Performances + current approval:'), writeln(R).

robot_current_query(N) :-
	distributionalclause:eval_query_backward_distrib([],[],NH,nrobots~=NH,N,D),
	write('Number robots:'), writeln(D),
	
	eval_query_particle2(C6,current(has_category(pr2_ca,robot(1)))~=C6, N, R6),
	write('Is pr2:'), writeln(R6),
	eval_query_particle2(C3,current(has_category(robots_ca,robot(1)))~=C3, N, R3),
	write('Is robot:'), writeln(R3),
	eval_query_particle2(C2,current(has_category(actionperformers_ca,robot(1)))~=C2, N, R2),
	write('Is actionperf:'), writeln(R2),
	eval_query_particle2(Ca,current(has_category(anything_ca,robot(1)))~=Ca, N, Ra),
	write('Is anything:'), writeln(Ra),
	eval_query_particle2(Ch,current(has_category(humans_ca,robot(1)))~=Ch, N, Rh),
	write('Is human:'), writeln(Rh),
	eval_query_particle2(C1,current(has_category(cups_ca,robot(1)))~=C1, N, R1),
	write('Is cup:'), writeln(R1),

	eval_query_particle2(H11,current(holding_object(left_gripper))~=H11, N, H1),
	write('Holding left gripper:'), writeln(H1),
	eval_query_particle2(H22,current(holding_object(right_gripper))~=H22, N, H2),
	write('Holding right gripper:'), writeln(H2),

	
	eval_query_particle2(C13,current(handling_command)~=C13, N, R13),
	write('Handling command:'), writeln(R13),
	eval_query_particle2(C11,current(waiting_answer)~=C11, N, R11),
	write('Waiting for answer:'), writeln(R11),
	eval_query_particle2(C12,current(answer)~=C12, N, R12),
	write('Possible answers:'), writeln(R12),nl.

need_current_query(H,N) :-
	eval_query_particle2(Need,current(has_need(H))~=Need, N , P1),
	write('Current need for '),write(H),write(' :'),writeln(P1).

command_current_query(C,N) :-
	eval_query_particle2(Cat,current(comm_category(C))~=Cat , N , P1),
	write(C),write(' currently: '),writeln(P1).

wanted_actioninfo(H,N) :-
	eval_query_particle2(B,current(wants_action(H))~=B , N , P1),
	write(H),write(' wants action: '),writeln(P1),

	eval_query_particle2(AC,current(action_category(H))~=AC , N , P2),
	write(H),write(' wants action-type: '),writeln(P2),

	eval_query_particle2(F,current(has_focus(H))~=F , N , P3),
	write('Has focus: '),writeln(P3).

	/*
	distributionalclause:eval_query_backward_distrib( [] , [] ,(B,WA,AC,F) , (,has_focus(WA,T)~=F) , 100 , P1),
	%,action_category(WA,T)~=AC,has_focus(WA,T)~=F,B\==false,AC\==none,F\==none,S\==none
	%,has_start(WA,T)~=S,B\==false,AC\==none,F\==none,S\==none
	write(H),write(' wants action at time '),write(T),write(' :'),writeln(P1)
	*/
	
scenario1 :-	
	

	%extract focus constraints
	%TODO in procedure steken
	
	add_evidence(distributionalclause(focus_constraints(O,comm1),finite([1:true]),true,1)),

	%best_actionperformance(1,action0,APerf),
	/*
	nl,
	writeln('User gives "object2" command'),
	evidence(gives_command(human1,pr2robot1,comm2, 1)),
	evidence(distributionalclause( has_category(commands_ca,comm2,0),finite([1:true]),true,1)),
	evidence(command_content(comm2,[object2])),%TODO object description hier

	evidence(timepoint(2)),
	write('time is 2'),
	need_info(human1,2),	
	comm_info(comm2,2),nl,
	*/
	%TODO problemen met comm2 wordt onnodig gekoppeld aan has_focus, moet enkel comm1 zijn
	
	%best_actionperformance(2,action0,APerf2), %TODO

	nl,
	writeln('Wanted action uncertainty, evolution over time'),
	%wanted_actioninfo(human1,0),
	wanted_actioninfo(human1,1).
	%wanted_actioninfo(human1,2),nl .

	%%%.
	%voorlopig gewoon argmax
	
s2 :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),

	nl,writeln('User gives "pick up red object" command (should give object 1 and 3 as possibility)'),nl,
	Content = [pickup(O),distributionalclause(current(has_colour(O)),finite([1:red]),true,1)],
	find_object_actedon_clues(Content,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	add_evidence(distributionalclause(current(focus_constraints(O,command(1))),finite([1:true]),(Tu),1)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),Content))],[],N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),
	
	best_actionperformance(human(1),APerf1,N),

	nl,writeln('User gives "1" answer'),nl,
	step_particle([action(answer(focus,choice,1))],[],N),
	robot_current_query(N),
	wanted_actioninfo(human(1),N),
	best_actionperformance(human(1),APerf2,N),
	APerf2 = [Name,Cat,F,[ExtraParams]],
	writeln(APerf2),
	add_evidence(perf(Name,Cat,F,[ExtraParams])),
	performance_current_query(N),
	nl,writeln('Robot starts performing'),
	step_particle([action(start_perf(Name))],[],N),
	performance_current_query(N),
	obj_current_query(object(1),N),
	obj_current_query(object(3),N),
	nl,writeln('Robot indicates successful performance'),
	step_particle([action(success_perf(Name))],[],N),
	performance_current_query(N),
	obj_current_query(object(1),N),
	obj_current_query(object(3),N),
	nl,writeln('Human indicates wanted performance'),
	step_particle([action(feedback(Name,good))],[],N),
	performance_current_query(N)
	.


s3 :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),

	nl,writeln('User gives "pick up cup" command (should give object 1>3>none as possibilities)'),nl,
	Content = [pickup(O),distributionalclause(current(has_category(cups_ca,O)),finite([1:true]),true,1)],
	find_object_actedon_clues(Content,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	add_evidence(distributionalclause(current(focus_constraints(O,command(1))),finite([1:true]),(Tu),1)),
	%TODO maak deftige command processing waarbij naam wordt toegewezen enzo
	add_evidence(comm_content(command(1),Content)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),Content))],[],N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N).

	%best_actionperformance(1,action0,APerf).

s4 :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),

	nl,writeln('User gives "red object" command '),nl,
	Content = [distributionalclause(current(has_colour(O)),finite([1:red]),true,1)],
	find_object_actedon_clues(Content,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	add_evidence(distributionalclause(current(focus_constraints(O,command(1))),finite([1:true]),(Tu),1)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),Content))],[],N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N).

	%best_actionperformance(1,action0,APerf).

s5water :- 
	N is 200,
	init_particle(N),
	cupsworld3re(N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),

	nl,writeln('User gives "pick up water" command '),nl,
	Content = [pickup(O), distributionalclause(current(has_category(water_ca,O)),finite([1:true]),true,1)],
	find_object_actedon_clues(Content,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	add_evidence(distributionalclause(current(focus_constraints(O,command(1))),finite([1:true]),(Tu),1)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),Content))],[],N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N).

	%best_actionperformance(1,action0,APerf).

s5cola :- 
	N is 200,
	init_particle(N),
	cupsworld3re(N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),

	nl,writeln('User gives "pick up cola" command '),nl,
	Content = [pickup(O), distributionalclause(current(has_category(cola_ca,O)),finite([1:true]),true,1)],
	find_object_actedon_clues(Content,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	add_evidence(distributionalclause(current(focus_constraints(O,command(1))),finite([1:true]),(Tu),1)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),Content))],[],N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N).

	%best_actionperformance(1,action0,APerf).

s6 :-

	N is 200,
	init_particle(N),
	cupsworld3re(N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),

	nl,writeln('User gives "pick up left cup" command '),nl,
	Content = [pickup(O),distributionalclause(current(has_category(cups_ca,O)),finite([1:true]),true,1),distributionalclause(current(leftof(O,O2)),finite([1:true]),true,1)],
	find_object_actedon_clues(Content,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	add_evidence(distributionalclause(current(focus_constraints(O,command(1))),finite([1:true]),(Tu),1)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),Content))],[],N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N).

	%best_actionperformance(1,action0,APerf).

s7 :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),

	nl,writeln('User gives "pick up right cup" command '),nl,
	Content = [pickup(O),distributionalclause(current(has_category(cups_ca,O)),finite([1:true]),true,1),distributionalclause(current(rightof(O,O2)),finite([1:true]),true,1)],
	find_object_actedon_clues(Content,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	add_evidence(distributionalclause(current(focus_constraints(O,command(1))),finite([1:true]),(Tu),1)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),Content))],[],N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N).

	%best_actionperformance(1,action0,APerf).

s8 :-
	N is 200,
	init_particle(N),
	cupsworld3re(N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N),

	nl,writeln('User gives "pick up cup near soda can" command '),nl,
	Content = [pickup(O),distributionalclause(current(has_category(cups_ca,O)),finite([1:true]),true,1),distributionalclause(current(has_category(sodacans_ca,O2)),finite([1:true]),true,1),distributionalclause(current(near(O,O2)),finite([1:true]),true,1)],
	find_object_actedon_clues(Content,O,E),
	convert_to_query(E,E3),
	list_to_tuple(E3,Tu),
	add_evidence(distributionalclause(current(focus_constraints(O,command(1))),finite([1:true]),(Tu),1)),

	step_particle([action(gives_command(human(1),pr2robot1,command(1))),action(command_content(command(1),Content))],[],N),

	command_current_query(command(1),N),
	need_current_query(human(1),N),
	wanted_actioninfo(human(1),N).

	%best_actionperformance(1,action0,APerf).

%"red object" by human 1 (wants actions) %TODO verder uitwerken
%"red object" by uncertain human id -> trigger question

%"object1 near object2 at T=1" ok
%near(object1,object2,T), future(T)

%"object1content near object2 at T=1" human wanting actions - ok

%"black object near object2" ok
%(common sense dat object1 niet al bij object2 mag staan (afh van vraag of niet), dit zit er al in met near anders bij T1 en T2???)

%"water near cola" ok
%2 vars moeten verschillend zijn!!!!!! ook in andere queries
% 3.516 CPU in 6.534 seconds ( 99% CPU)

position_query(O) :-
	timepoint(T),
	position_query(O,T).

position_query(O,T) :-
	distributionalclause:eval_query_backward_distrib([],[],(X,Y,Z),(has_position(O,T)~=(X,Y,Z)),1000,P),		
	write(O),
	write(' has position(s) at time '),write(T),
	write(' with probabilities: '),writeln(P).

ask_focus(OCat,ACat,Options) :-
	unique_object_descriptions(Options,Ds),
	nl,write('What '),
	insert_categoryname(OCat),
	write('did you want to '),
	insert_actionmention(ACat),
	writeln('?'),
	ask_option(Ds),
	writeln(-------------).

ask_actiontype(ACats,ChosenA) :-
	writeln('Which action did you want performed?'),
	ask_actionoptions(1,ACats,ChosenN),
	nth1(ChosenN,ACats,ChosenA),
	writeln(-------------).
ask_actionoptions(_,[],ChosenN) :-
	read(ChosenN).
ask_actionoptions(N,[pickup_ca|R],ChosenN) :-
	write('Pick up. '),write('(type '), write(N),writeln('.)'),
	N2 is N+1,
	ask_actionoptions(N2,R,ChosenN).
ask_actionoptions(N,[pickandplace_ca|R],ChosenN) :-
	write('Pick and place. '),write('(type '), write(N),writeln('.)'),
	N2 is N+1,
	ask_actionoptions(N2,R,ChosenN).

ask_commandtype(CommCats,Chosen) :-
	writeln('What did you want performed?'),
	ask_commandoptions(1,CommCats,ChosenN),
	nth1(ChosenN,CommCats,Chosen),
	writeln(-------------).
ask_commandoptions(_,[],ChosenN) :-
	read(ChosenN).
ask_commandoptions(N,[answerquestioncommands_ca|R],ChosenN) :-
	write('Answer my question. '),write('(type '), write(N),writeln('.)'),
	N2 is N+1,
	ask_commandoptions(N2,R,ChosenN).
ask_commandoptions(N,[performphysicalactioncommands_ca|R],ChosenN) :-
	write('Perform action. '),write('(type '), write(N),writeln('.)'),
	N2 is N+1,
	ask_commandoptions(N2,R,ChosenN).
%TODO give_evidence alternatief

ask_option(Options) :-
	%writeln(Options),
	give_options(1,Options).
	
	/*
	read(N),
	nth1(N,Info,InfoN),
	write('You have chosen the '),
	insert_colour(InfoN),
	insert_categoryname(Cat),
	writeln('.'),
	nth1(N,Options,Chosen).
	*/

ask_focus_diff(pickup_ca,FChoice) :-
	write('Which of these do you want to pickup? '), writeln(FChoice).
	%TODO duidelijke eigenschappen voor persoon

has_colour([H|R],Colour) :-
	%writeln(H),
	can_be(H,colour_ca),
	Colour=H.
has_colour([H|R],Colour) :-
	has_colour(R,Colour).

insert_colour(InfoN) :-
	(has_colour(InfoN,Colour) ->		
		write(Colour),
		write(' ')
	;
		write('')
	).

insert_categoryname(Cat) :-
	naturalname(Cat,Name),
	write(Name),write(' ').

insert_actionmention(ACat) :-
	naturalname(ACat,AName),
	write(AName),write(' ').

naturalname(moveablethings_ca,'object').
naturalname(cups_ca,'cup').
naturalname(pickup_ca,'pick up').
%etc... 	

give_options(_,[]).
give_options(N,[H2|R2]) :-
	/*
	is_subcategory(OCat,moveablethings_ca),
	write('The '),
	insert_colour(H2),
	insert_categoryname(OCat),
	write('(type '), 
	*/

	write(N),write(': '),
	%writeln('.)'),
	%writeln(H),
	writeln(H2),
	N2 is N+1,
	give_options(N2,R2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%could model which user wants robot questions preferrably or not
%"Pick up the table" %TODO good answer but unnecessary backtrack + common sense + niet object oppikken met anderen op
e2 :- init, execute_command([distributionalclause(actioncategory(p1),finite([1:pickup_ca]),true,1), distributionalclause(has_category(O),finite([1:tables_ca]),true,1),distributionalclause(acts_on_object(p1,O),finite([1:true]),true,1), distributionalclause(has_performer(p1),finite([1:pr2robot1]),true,1)], distributionalclause(commandgiver(c1),finite([0.9:human1,0.05:human2,0.05:unknownhuman]),true,1)).

%"O near object2 at T=0" human wanting answers - ok
near22 :- 
	init,
	execute_command([distributionalclause(near(O,object2,0),finite([1:true]),true,1)], distributionalclause(commandgiver(c1),finite([0.1:human1,0.8:human2,0.1:unknownhuman]),true,1)).

%"Move the water" %%TODO
e4 :- init,execute_command([instance2(Water, water_ca), instance2(move1, movesomething_ca),movesomething(move1, Water)], distributionalclause(commandgiver(c1),finite([0.9:human1,0.05:human2,0.05:unknownhuman]),true,1)).

%filtering
%step_particle, it performs 1 step:
%step_particle(ListActions,ListEvidence,NParticles).

%prediction
%step_particle without evidence

%for queries in DCPF:
%eval_query_particle(query,Nparticles,Result).

%for queries in DC:
%eval_query

%meerdere acties eventueel
