%%% -*- Mode: Prolog; -*-
:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module('knowledge_base.pl').
:- use_module('command_processing.pl').
:- use_module('color_processing.pl').
:- use_module('perception_module.pl').
:- use_module('knowledge_processing.pl').
:- use_module('action_decision.pl').
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- set_magic(backward). 

lifted(false).
raoblackwellisation(false).

objectsObservationIterationParticles(NumberRecognized,Info,N) :-
	nl,writeln('Previous number objects knowledge:'),
	nobjs_current_query(N),

	step_particle([],[observation(nobjsit)~=NumberRecognized],N),

	nl,writeln('New number objects knowledge:'),
	nobjs_current_query(N),

	nl,write('Processing new scan information for each particle: '),writeln(Info),
	
	processTypeInfo(Info,Info2),
	step_particle4(Info2,NumberRecognized,N),
	dcpf:resampling(N)
	.
	

processTypeInfo([],[]).
processTypeInfo([H2|R2],[H3|R3]) :-
	H2 = [Xmid,Ymid,Zmid,Xr,Yr,Zr,Red,G,B,Types],
	%colourProbability((Red,G,B),P),%NP
	%writeln(colors_recognized:P),
	modelnumbersToCats(Types,Types2),
	probabilities_sum(Types2,Sum),
	relative_probs(Types2,Sum,RelProbs),
	H3 = [Xmid,Ymid,Zmid,Xr,Yr,Zr,Red,G,B,RelProbs],
	processTypeInfo(R2,R3).

objectsObservationIteration2(PreviousOnTable,NumberRecognized,Info,I,ObsT,[],[]) :-
	length(PreviousOnTable,Le1),
	Le1 == NumberRecognized,
	%writeln('Same amount of objects on table.'),

	findall(ObjectID,member((object(ObjectID)),PreviousOnTable),PreviousOnTableIDs),
	%write('object ids on table previous: '), writeln(Pre),
	calculateDistances(PreviousOnTableIDs,Info,Matrix,I),

	findall((Sub,D),(permutation(PreviousOnTableIDs,Pre2),length(Sub,NumberRecognized),member(Nu,Pre2),member(Nu,Sub),sublist(Sub,Pre2),distanceMatrix(Sub,Matrix,D)),L),
	remove_duplicates(L, Lpruned),
	findall(Score,member((SS,Score),Lpruned),ScoreL),
	min_list(ScoreL,Min),
	member((Sub1,Min),Lpruned),
	objectsPositionUpdate(Sub1,Info,I,ObsT),!.

objectsObservationIteration2(NO1,NO2,Info,I,Obs,Ev,Actions) :-
	length(NO1,Le1),
	Le1>NO2,
	%writeln('Object(s) removed from table'),

	findall(ObjectID,member((object(ObjectID)),NO1),Pre),
	%write('object ids on table previous: '), writeln(Pre),
	calculateDistances(Pre,Info,Matrix,I),

	findall((Sub,D),(permutation(Pre,Pre2),length(Sub,NO2),member(Nu,Pre2),member(Nu,Sub),sublist(Sub,Pre2),distanceMatrix(Sub,Matrix,D)),L),
	remove_duplicates(L, Lpruned),
	findall(Score,member((SS,Score),Lpruned),ScoreL),
	min_list(ScoreL,Min),
	member((Sub1,Min),Lpruned),
	objectsPositionUpdate(Sub1,Info,I,Obs),
	getRemoved(Pre,Sub1,Rem),
	objectsRemoved(Rem,I,Ev,Actions),!.

%TO CHECK test ok
objectsObservationIteration2(PreviousOnTable,NumberRecognized,Info,I,ObsT,[],Actions) :-
	length(PreviousOnTable,Le1),
	Le1<NumberRecognized,
	Diff is NumberRecognized-Le1,
	%writeln('Object(s) extra on table'),
	
	eraseall(tempparticle),
	findall(H,(distributionalclause:proof_query_backward(I,tempparticle,(current(seen(object(H))) ~= true))),ListS),
	%writeln(ListS),
	length(ListS,LengthS),
	LengthS < NumberRecognized,
	StartI is LengthS+1,
	findall(Num,between(StartI,NumberRecognized,Num),Lii),
	

	findall(ObjectID,member((object(ObjectID)),PreviousOnTable),Pre),
	length(Pre,PreLe),
	calculateDistances(Pre,Info,Matrix,I),
	numlist(1,NumberRecognized,List),
	findall((L11),(length(L11,PreLe),permutation(List,List2),sublist(L11,List2)),L2),
	remove_duplicates(L2, L2pruned),
	findall((L111,Di),(member(L111,L2pruned),distCalc(L111,1,PreLe,Matrix,Di)),LL),
	findall(Score,member((SS,Score),LL),ScoreL),
	min_list(ScoreL,Min),
	member((Sub1,Min),LL),
	processObservations(Sub1,Pre,Info,I,ObsT),
	pruneInfo(Info,Sub1,InfoPruned),
	eraseall(tempparticle),
	findall(O,(distributionalclause:proof_query_backward(I,tempparticle,(current(object(O))~=true))),ExistingObjects),
	getRemoved(ExistingObjects,Pre,Rem),
	findall((L4,Sim),(length(L1,Diff),sublist(L1,Rem),permutation(L1,L4),similarityScore(L4,InfoPruned,Sim,I)),L),
	remove_duplicates(L, Lpruned),
	findall(Score2,member((SS2,Score2),Lpruned),ScoreL2),
	min_list(ScoreL2,Min2),
	member((Sub2,Min2),Lpruned),
	objectsPlacedActions(Lii,InfoPruned,Actions,I),!.
	%writeln(Actions).


objectsObservationIteration2(PreviousOnTable,NumberRecognized,Info,I,ObsT,[],Actions) :-
	length(PreviousOnTable,Le1),
	Le1<NumberRecognized,
	Diff is NumberRecognized-Le1,
	%writeln('Object(s) extra on table'),

	findall(ObjectID,member((object(ObjectID)),PreviousOnTable),Pre),
	length(Pre,PreLe),
	%write('object ids on table same pos: '), writeln(Pre),
	calculateDistances(Pre,Info,Matrix,I),
	numlist(1,NumberRecognized,List),
	findall((L11),(length(L11,PreLe),permutation(List,List2),sublist(L11,List2)),L2),
	remove_duplicates(L2, L2pruned),
	findall((L111,Di),(member(L111,L2pruned),distCalc(L111,1,PreLe,Matrix,Di)),LL),
	findall(Score,member((SS,Score),LL),ScoreL),
	min_list(ScoreL,Min),
	member((Sub1,Min),LL),
	processObservations(Sub1,Pre,Info,I,ObsT),
	pruneInfo(Info,Sub1,InfoPruned),
	eraseall(tempparticle),
	findall(O,(distributionalclause:proof_query_backward(I,tempparticle,(current(object(O))~=true))),ExistingObjects),
	getRemoved(ExistingObjects,Pre,Rem),
	findall((L4,Sim),(length(L1,Diff),sublist(L1,Rem),permutation(L1,L4),similarityScore(L4,InfoPruned,Sim,I)),L),
	remove_duplicates(L, Lpruned),
	findall(Score2,member((SS2,Score2),Lpruned),ScoreL2),
	min_list(ScoreL2,Min2),
	member((Sub2,Min2),Lpruned),
	objectsPlacedActions(Sub2,InfoPruned,Actions,I),!.
	%writeln(Actions).
	
objectsPlacedActions([],[],[],I).
objectsPlacedActions([H|R],[H2|R2],HRRR,I) :-
	eraseall(tempparticle),
	distributionalclause:proof_query_backward(I,tempparticle,(current(seen(object(H))) ~= false)),
	%write(ID),write(' not seen yet in particle '),writeln(I),
	H2 = [Xmid,Ymid,Zmid,Xr,Yr,Zr,Red,G,B,Types],
	H31 = [action(human_placed(object(H),[Xmid,Ymid,Zmid]))],
	
	H32 = [action(objdims(object(H),[Xr,Yr,Zr]))],
	H33 = [action(segm(object(H),Types))],
	%colorObservationInit(ID,[R,G,B],N) %TODO probabil voor IDs in particles
	append(H31,H32,HR),
	append(HR,H33,HR2),
	objectsPlacedActions(R,R2,R3,I),
	append(R3,HR2,HRRR).

objectsPlacedActions([H|R],[H2|R2],[H31|R3],I) :-
	eraseall(tempparticle),
	distributionalclause:proof_query_backward(I,tempparticle,(current(seen(object(H))) ~= true)),
	%write(ID),write(' seen already in particle '),writeln(I),
	H2 = [Xmid,Ymid,Zmid,Xr,Yr,Zr,Red,G,B,Types],
	H31 = action(human_placed(object(H),[Xmid,Ymid,Zmid])),
	objectsPlacedActions(R,R2,R3,I).

objectsRemoved([],_,[],[]).
objectsRemoved([H|R],I,[H3|R3],[H1|R2]) :-
	eraseall(tempparticle),
	distributionalclause:proof_query_backward(I,tempparticle,(current(wanted_action(human(1))) ~= (pickup,object(H),Param))),%NP human doorgeven	
	H3 = observation(exists)~=false,	
	H1 = action(human_removed(object(H))),
	objectsRemoved(R,I,R3,R2). 
objectsRemoved([H|R],I,R3,[H1|R1]) :-
	H1 = action(human_removed(object(H))),
	objectsRemoved(R,I,R3,R1).

objectsPositionUpdate([],[],_,[]).
objectsPositionUpdate([H|R],[H2|R2],I,[H3|R3]) :-
	H2 = [Xmid,Ymid,Zmid,Xr,Yr,Zr,Red,G,B,Types],
	H3 = action(pos(object(H),[Xmid,Ymid,Zmid])),
	objectsPositionUpdate(R,R2,I,R3). 

processObservations([],[],_,_,[]).
processObservations([H|R],[ID|R2],Info,I,[H3|R3]) :-
	nth1(H,Info,InfoE),
	objectsPositionUpdate([ID],[InfoE],I,H3),	
	processObservations(R,R2,Info,I,R3).

getRemoved([],_,[]).
getRemoved([H|R],List,R3) :-
	member(H,List),
	getRemoved(R,List,R3).
getRemoved([H|R],List,[H|R3]) :-
	\+member(H,List),
	getRemoved(R,List,R3).

distCalc(L11,I,PreLe,Matrix,Di) :-
	I<PreLe,
	nth1(I,L11,E),
	nth1(E,Matrix,E2),
	nth1(I,E2,E3),
	I2 is I+1,
	distCalc(L11,I2,PreLe,Matrix,Di2),
	Di is Di2 + E3.	

distCalc(L11,PreLe,PreLe,Matrix,E3) :-
	nth1(PreLe,L11,E),
	nth1(E,Matrix,E2),
	nth1(PreLe,E2,E3).

similarityScore([],[],0,_).
similarityScore([ID|R],[Info1|R2],SimT,I) :-
	similarityScoreObject(ID,Info1,S1,I),
	similarityScore(R,R2,S2,I),
	SimT is S1 + S2.

similarityScoreObject(ID,Info1,S1,I) :-
	
	eraseall(tempparticle),
	distributionalclause:proof_query_backward(I,tempparticle,(current(has_dimensions(object(ID))) ~= [X1,Y1,Z1])),
	%has_dimensions(object(ID),[X1,Y1,Z1]),	
	Info1 = [Xmid,Ymid,Zmid,Xr,Yr,Zr,R,G,B,Types],	
	VolumeDiff is abs((X1*Y1*Z1)-(Xr*Yr*Zr)),
	VolumeSimil is 10/VolumeDiff,

	eraseall(tempparticle),	
	distributionalclause:proof_query_backward(I,tempparticle,(current(obj_category(object(ID))) ~= Cat)),
	findall(Prob,member((Prob:Cat),Types),Ltypes),
	sum_list(Ltypes,TypesSum),
	TypeSimil is 10000/TypesSum,
	S1 is TypeSimil + VolumeSimil.%TODO extend

pruneInfo(Info,Sub1,InfoPruned) :-
	length(Info,Le),
	pruneInfo2(1,Le,Sub1,Info,InfoPruned).

pruneInfo2(Le,Le,Inds,Info,[]) :-
	member(Le,Inds).
	
pruneInfo2(Le,Le,_,Info,[Inf]) :-
	nth1(Le,Info,Inf).

pruneInfo2(I,Le,Inds,Info,Info2) :-
	member(I,Inds),
	I2 is I+1,
	pruneInfo2(I2,Le,Inds,Info,Info2).
pruneInfo2(I,Le,Inds,Info,[Inf|Info2]) :-	
	nth1(I,Info,Inf),
	I2 is I+1,
	pruneInfo2(I2,Le,Inds,Info,Info2).

pos_wanted_effect(Name,N) :-
	nl, writeln('Human indicates action did have wanted effect'),
	step_particle([],[observation(wanted_feedback(Name))~=true],N).
neg_wanted_effect(Name,N) :-
	nl, writeln('Human indicates action did not have wanted effect'),
	step_particle([],[observation(wanted_feedback(Name))~=false],N).

end_performance(Name,N) :- 
	nl,writeln('Robot indicates end performance'),
	%writeln(Name),
	step_particle([action(end_perf(Name))],[],N),
	%writeln(endperf1),
	remove_evidence([performing(Name)]),
	%writeln(endperf2),
	add_new_evidence([latest_perform(Name)]),%NP updaten en removen
	%writeln(endperf3),
	performance_current_query(N).
	%eval_query_particle2((Eff),current(effect(Name))~=Eff,N,R2e).
	
	%writeln(R2e),
	%writeln(endperf4),
	%robot_current_query(N),
	%writeln(endperf5).

distanceMatrix([],[],0).
distanceMatrix([S1|Sr],[D1|Rest],Dr) :-
	nth1(S1,D1,Dr1),
	distanceMatrix(Sr,Rest,Dr2),
	Dr is Dr1 + Dr2.

calculateDistances(_,[],[],_).
calculateDistances(Pre,[[Xmid,Ymid,Zmid,Xr,Yr,Zr,R,G,B,Types]|R2],[H3|R3],I) :-
	calculateDistances2(Pre,(Xmid,Ymid,Zmid),H3,I),
	calculateDistances(Pre,R2,R3,I).
	
calculateDistances2([],_,[],_).
calculateDistances2([H|R],ObjLoc,[Distance|R2],I) :-
	eraseall(tempparticle),
	distributionalclause:proof_query_backward(I,tempparticle,(current(has_position(object(H)))~=C11)),
	distance(ObjLoc,C11,Distance),
	calculateDistances2(R,ObjLoc,R2,I).

objids_current_query(IDs,N) :-
	eval_query_particle2((O),current(object(O))~=true,N,R2e),
	writeln(R2e),
	findall(ObjectID,member((_:ObjectID),R2e),IDs).

objontable_current_query(R2e,N) :-
	eval_query_particle2((O),current(on(O,table))~=true,N,R2e),
	writeln(R2e),
	length(R2e,NO1),
	writeln(NO1).

query_effect(Name,N) :-
	eval_query_particle2((B),current(wanted_effect(Name))~=B,N,R),
	write('performance '),write(Name),write(' has wanted effect '),writeln(R).

needBeginObservation(H,N) :-
	step_particle([action(needbegin(H))],[],N),
	craving_current_query(H,N),
	need_current_query(H,N).

colorObservationInit(ID,[R,G,B],N) :-
	colourProbability((R,G,B),P),
	%step_particle([action(col(object(ID),P))],[],N), 	
	findall(Col,member((_:Col),P),ListC),
	Factor is 1,
	R2 is R*Factor,
	G2 is G*Factor,
	B2 is B*Factor,
	/*
	writeln(R2),
	writeln(G2),
	writeln(B2),
	*/
	add_new_evidence([has_physical_generalRGB(object(ID),[R,G,B])]),
	colorObservationInit2(ID,ListC),	
	obj_colour_current_query(object(ID),N).
colorObservationInit2(_,[]).
colorObservationInit2(ID,[C1|CT]) :-
	add_new_evidence([has_colour(object(ID),C1)]),
	colorObservationInit2(ID,CT).

gripper_location(Gripper,[X,Y,Z],N) :- 
	%step_particle([action(pos(Gripper,[X,Y,Z]))],[],N).
	remove_evidence([has_position(Gripper,[X2,Y2,Z2])]),
	add_new_evidence([has_position(Gripper,[X,Y,Z])]).
start_performance(Name,N) :-
	nl,writeln('Robot starts performing'),
	add_new_evidence([performing(Name)]),
	step_particle([action(start_perf(Name))],[],N),
	perform_question(Name,N),
	performance_current_query(N).

perform_question(Name,N) :-
	perf(Name,toolq,_),
	eval_query_particle2((B2e),current(wanted_action(human(1)))~=B2e,N,R2e),
	writeln(R2e),
	askQuestion(tool,R2e,N).
perform_question(Name,N) :-
	perf(Name,typeq,_),
	eval_query_particle2((B2e),current(wanted_action(human(1)))~=B2e,N,R2e),
	askQuestion(type,R2e,N).
perform_question(Name,N) :-
	perf(Name,objectq,_),	
	eval_query_particle2((B2e),current(wanted_action(human(1)))~=B2e,N,R2e),
	writeln(R2e),
	askQuestion(object,R2e,N).
perform_question(Name,N) :-
	perf(Name,_,_).

success_performance(Name,N) :- 
	nl,writeln('Robot indicates successful performance'),
	step_particle([action(success_perf(Name))],[],N),
	remove_evidence([performing(Name)]),
	add_new_evidence([latest_perform(Name)]),%NP updaten en removen
	performance_current_query(N),
	robot_current_query(N).

/*
Feedback sectie.
*/
positive_feedback(PName,N) :-
	perf(PName,Cat,Params),
	Params = [O,_,_,_,Tool],
	nl,writeln('Human indicates this was wanted performance.'),
	step_particle([action(feedback(PName,good))],[],N),  
	step_particle([],[observation(posfeedback(human(1)))~=(Cat,O,Tool)],N),%NP human
	/*
	list_to_tuple([current(wanted_action(human(1)))~=(Cat,O,Tool)],Q2),
	get_variables(Q,L2),
	flatten(L2,L3),
	remove_duplicates(L3,L4),
	list_to_tuple(L4,L5),
	step_particle3(L5,Q2,N),
	*/	
	current_wa_query(human(1),N),
	performance_current_query(N).

negative_feedback(N,CA) :- 
	performance_current_query(N),
	latest_perform(Pname),
	perf(Pname,Cat,[O,X,_,_,Tool]),
	nl,writeln('Human indicates this was not wanted performance.'),
	step_particle([action(feedback(Pname,bad))],[],N), 
	perf(Pname,Cat,Params),
	neg_feedback_update((Cat,O,Tool),N),
	dcpf:resampling(N),
	current_wa_query(human(1),N),%NP
	write('Action to compensate is: '),write(Pname),write(','),write(Cat),write(','),write(Params),nl,
	compensation_action([Pname,Cat,Params],(CompPName,Cat2,Params2),N),
	timestep(Time),
	get_perfname(Time,Name),
	CA = (Name,Cat2,Params2),
	add_new_evidence([perf(Name,Cat2,Params2)]),
	write('Compensation action is: '),write(CA),
	current_wa_query(human(1),N).%NP

current_wa_query(He,N) :-
	eval_query_particle2((He,B2e),current(wanted_action(He))~=B2e,N,R2e),
	nl,write('Current wanted action distribution for '),write(He),write(' : '),writeln(R2e).
	%eval_query_particle2((O),current(wants_to_move(He,object(O)))~=B2e2,N,R2e2),
	%nl,write('Human  '),write(He),write(' wants a move of objects : '),writeln(R2e2).
	
typeObservationIt(ObjLoc,Tag,N) :-
	nl,
	write('A '),write(Tag),write(' has been detected at location '),write(ObjLoc),writeln('.'),
	eval_query_particle2(B,current(nobjs)~=B, N, R),
	%writeln(R),
	%R = [1.0:NO], NP
	%writeln(NO),
	distances(ObjLoc,NO,N,Dists),
	%writeln(Dists),
	min_list(Dists,Min),
	%writeln(Min),
	nth1(I,Dists,Min),
	%writeln(I),
	length(Dists,Length),
	ID is Length+1-I,
	%writeln(ID),
	write('Object identified: ID = '), write(ID),writeln('.'),
	step_particle([],[obsProbability(object(ID))~=Tag],N),
	%action(obsProb(object(ID),L)),
	%TODO	
	%writeln(L),
	step_particle([action(obsProb(object(ID),L))],[],N),
	obj_current_query(object(ID),N),nl.

%TODO zet in knowledge_processing

distances(_,0,_,[]).
distances(ObjLoc,NO,N,[H|R]) :-
	eval_query_particle2(C11,current(has_position(object(NO)))~=C11, N, R11),
	%eval_average_particle(Avg,C11,current(has_position(object(NO)))~=C11, N, R11),
	%averageobject(NO,N,Mean),	
	%writeln(average: Mean),
	R11 = [1.0:Loc],%TODO
	%writeln(Loc),
	distance(ObjLoc,Loc,H),
	%writeln(H),
	NO2 is NO-1,
	distances(ObjLoc,NO2,N,R).


	

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
		printp22(I),
		fail;
		true
	).

initdcpf(N) :-
	write('Initializing dcpf with '),write(N),writeln(' particles'),
	init_particle(N),
	writeln('Dcpf ready.').

nobjsObservationInit(NO,N) :-
	nl,
	writeln('New number of objects observation.'),
	nobjs_current_query(N),
	step_particle([],[observation(nobjsinit)~=NO],N),
	nobjs_current_query(N),
	seen_query(N),
	seenUpdate(NO,N),
	seen_query(N).

seenUpdate(0,_).
seenUpdate(Number,N) :-
	step_particle([action(seen(object(Number)))],[],N),
	Number2 is Number-1,
	seenUpdate(Number2,N).

seen_query(N) :-
	eval_query_particle2((NO,B),current(seen(object(NO)))~=B, N, R2),
	writeln(R2).

modelnumbersToCats([],[]).
modelnumbersToCats([(Score:Number)|R],[(Score2:Category)|R2]) :-
	model(Category,Number,Descr),
	numbermodels(Category,N2),
	Score2 is Score/N2,
	modelnumbersToCats(R,R2).

typeObservationInit(Obj,Cats,N) :-
	modelnumbersToCats(Cats,Cats2),
	probabilities_sum(Cats2,Sum),
	relative_probs(Cats2,Sum,RelProbs),
	%writeln(RelProbs),
	nl,
	write('New first type observation for obj '),write(Obj),write('.'),nl,
	step_particle([action(segm(object(Obj),RelProbs))],[],N), 
	%findall(X,member((_:X),Cats2),L),
	%remove_duplicates(L,L2),
	%step_particle([action(obsProb(object(Obj),L2))],[],N),
	obj_category_current_query(object(Obj),N).
	%R = action(obsProb(object(Obj),L2)),
	%asserta(R).
	
locationObservationIt(Obj,ObjLoc,N) :-
	nl,
	write('New place observation for obj '),write(Obj),write('.'),nl,
	step_particle([action(pos(object(Obj),ObjLoc))],[],N),%TODO
	obj_current_query(object(Obj),N).

locationObservationInit(Obj,ObjDim,ObjLoc,N) :-	
	nl,
	write('New first place observation for obj '),write(Obj),write('.'),nl,	
	step_particle([action(pos(object(Obj),ObjLoc))],[],N),%TODO
	%add_new_evidence([has_dimensions(object(Obj),ObjDim)]),
	step_particle([action(objdims(object(Obj),ObjDim))],[],N),
	obj_position_current_query(object(Obj),N).

tableObservation(TableDim,TablePos,N) :-
	nl,	
	writeln('Table observation.'),
	step_particle([action(dim(table,TableDim))],[],N),
	%writeln(has_dimensions(table,TableDim)),
	%add_new_evidence([has_dimensions(table,TableDim)]),		
	step_particle([action(pos(table,TablePos))],[],N),%TODO
	table_current_query(table,N).	

actionDemandObservation(Human,N) :-
	nl,	
	writeln('Human indicates an action is needed, can be following initially:'),
	%writeln(-----------------------),
	step_particle([action(wabegin)],[],N),
	step_particle([],[],N),
	current_wa_query(Human,N).

commandObservation(Human,[],N) :-
	writeln('Human gives non verbal sign'), 
	writeln('Estimating new wanted action distribution:'),
	current_wa_query(Human,N).

commandObservation(Human,Q,N) :- 
	writeln(Q),
	list_to_tuple(Q,Q2),
	get_variables(Q,L2),
	L2 == [[]],
	writeln('ok'),
	step_particle3([],Q2,N),
	nl,	
	write('Human gives command: '),writeln(Q), 
	writeln('Estimating new wanted action distribution:'),
	current_wa_query(Human,N).

commandObservation(Human,Q,N) :- 
	writeln(Q),
	list_to_tuple(Q,Q2),
	get_variables(Q,L2),
	writeln(l2:L2),
	flatten(L2,L3),
	remove_duplicates(L3,L4),
	list_to_tuple(L4,L5),
	writeln(l5:L5),
	eval_query_particle2(L5,Q2,N,R2),
	write('Command queried on knowledge:'), writeln(R2),
	R2 \= [],
	writeln('Command makes sense.'),
	step_particle3(L5,Q2,N),
	nl,	
	write('Human gives command: '),writeln(Q), 
	writeln('Estimating new wanted action distribution:'),
	current_wa_query(Human,N).
commandObservation(Human,Q,N) :-
	writeln('Command makes no sense!!!!!').

nobjs_current_query(N) :-
	eval_query_particle2(B,current(nobjs)~=B, N, R2),
	write('Number of objects status:'), writeln(R2).

obj_position_queries([],_).
obj_position_queries([H|R],N) :-
	nl,
	write('Object '),writeln(H),
	obj_position_current_query(object(H),N),
	obj_position_queries(R,N).

obj_queries([],_).
obj_queries([H|R],N) :-
	nl,
	obj_current_query(object(H),N),
	obj_queries(R,N).

obj_current_query(object(NO),N) :-
	write('Object '),write(NO),writeln(' status:'),
	obj_category_current_query(object(NO),N),
	obj_position_current_query(object(NO),N),
	obj_dimensions_current_query(object(NO),N),
	obj_colour_current_query(object(NO),N),
	obj_reachable_current_query(object(NO),N).

obj_reachable_current_query(object(NO),N) :-
	eval_query_particle2((B),current(reachable(object(NO),left_gripper))~=B, N, R1),
	write('Object reachable with left gripper: '), writeln(R1),
	eval_query_particle2((B2),current(reachable(object(NO),right_gripper))~=B2, N, R2),
	write('Object reachable with right gripper: '), writeln(R2).

obj_exists_current_query(object(NO),N) :-
	eval_query_particle2(B,current(object(NO))~=B, N, R2),
	write('Object exists: '), writeln(R2),nl.

obj_category_current_query(object(NO),N) :-
	eval_query_particle2(C,current(obj_category(object(NO)))~=C, N, R1),
	write('Object category: '), write(R1),nl,
	eval_query_particle2(C1,current(holds_fluid(object(NO)))~=C1, N, R11),
	write('Holds fluid: '), write(R11),nl.

obj_position_current_query(object(NO),N) :-
	eval_query_particle2(C11,current(has_position(object(NO)))~=C11, N, R11),
	write('Object position: '), write(R11),nl.

obj_dimensions_current_query(object(NO),N) :-
	eval_query_particle2(Cd,current(has_dimensions(object(NO)))~=Cd, N, Rd),
	%has_dimensions(object(NO),Rd),	
	write('Object dimensions:'), write(Rd),nl.

obj_colour_current_query(object(NO),N) :-
	%eval_query_particle2(L2,current(has_colour(object(NO)))~=L2, N, R111),
	findall(Col,has_colour(object(NO),Col),Cols),	
	write('Object colour object '),write(NO),write(' :'), writeln(Cols).

table_current_query(table,N) :-
	eval_query_particle2(C11,current(has_position(table))~=C11, N, R11),
	write('Table position:'), writeln(R11),

	eval_query_particle2(Cd,current(has_dimensions(table))~=Cd, N, Rd),
	%has_dimensions(table,Rd),		
	write('Table dimensions:'), writeln(Rd).

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
	%eval_query_particle2(P11,current(performing)~=P11, N, P1),
	findall(Perf,performing(Perf),Perfs),	
	write('Performing:'), writeln(Perfs),
	eval_query_particle2((Perf2,C),current(good_perform(Perf2))~=C, N, R),
	write('Performances + current approval:'), writeln(R).

robot_current_query(N) :-
	eval_query_particle2(H11,current(holding_object(left_gripper))~=H11, N, H1),
	write('Holding left gripper:'), writeln(H1),
	eval_query_particle2(H22,current(holding_object(right_gripper))~=H22, N, H2),
	write('Holding right gripper:'), writeln(H2).

need_current_query(H,N) :-
	eval_query_particle2(Need,current(has_need(H))~=Need, N , P1),
	nl,write('Current need for '),write(H),write(' :'),writeln(P1).

craving_current_query(H,N) :-
	eval_query_particle2(Hunger,current(has_craving(H,hunger))~=Hunger, N , P1),
	nl,write('Current craving for food:'),write(H),write(' :'),writeln(P1),
	
	eval_query_particle2(Thirst,current(has_craving(H,thirst))~=Thirst, N , P2),
	nl,write('Current craving for a drink:'),write(H),write(' :'),writeln(P2)
	.

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

neg_feedback_update(NegAction,N) :-
	
	Delta is 1.0,
	%statistics(cputime,[TimeInit,_]),

	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	dcpf:bb_get(offset,Offset),
	dcpf:bb_put(likelihood,0.0),
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	%write('timestep '),write(T),nl,
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		dcpf:abolish_all_tables,
		eraseall(tempparticle),
		findall(H,(distributionalclause:proof_query_backward(I,tempparticle,(current(wanted_action(human(H)))~=NegAction))),List),
		
		%nl,
		length(List,LengthLL),
	
		(  LengthLL == 0 ->
			PosEvidence = [observation(exists)~=true],
			assert_list(I,PosEvidence) % assert observations
		;
			PosEvidence = [observation(exists)~=false],
			assert_list(I,PosEvidence)
		),
		
		dcpf:bb_get(I,LogTempWeight),
		TempWeight is exp(LogTempWeight),
		TempWeight>0.0,
		dcpf:bb_get(initdcpf,InitDCPF),
		(
			get_magic(true) ->
			(
				init_query(I,next(_)),
				init_query(I,next(_) ~= _),
				init_query_list(I,PosEvidence)
			)
			;
			true
		),
		
		(
			get_magic(backward) ->
			(
				user:raoblackwellisation(false) ->
				(	
					dcpf:logweight_constraints(10,I,PosEvidence,Constraints,P)
				)
				;
					true
			)
			;
			true
		),
		(
			(InitDCPF==false) ->
			(
				get_magic(backward) ->
					dcpf:inferencestep_particlefilter_backward3(I)
				;
					dcpf:generate_sample_particlefilter(I,MaxP)
			)
			;
			true
		),
		
		(
			user:raoblackwellisation(true) ->
			(
				add_rao_backward(I,PosEvidence),
				inferencestep_particlefilter_magicoff_rao(I,_),
				(
					get_magic(backward) ->
						inferencestep_particlefilter_backward2(I)
					;
						generate_sample_particlefilter(I,MaxP)
				)
			)
			;
				true
		),
		
		(
			get_magic(true) ->
				eval_weight(I,PosEvidence,P)
			;
			(
				user:raoblackwellisation(true) ->
				(
					nl,nl,write('particle '),write(I),nl,
					findall(Sum,
						(
							recorded(I,next(VarRao) ~= finite(Distr),R),
							user:rao(VarRao),
							
							write(next(VarRao) ~= finite(Distr)),nl,
							findall(NewP:ValRao,(
										member(Pval:ValRao,Distr),
										
										eval_weightRao(I,PosEvidence,WRao,VarRao,ValRao),
										write(eval_weightR(I,PosEvidence,WRao,VarRao,ValRao)),nl,
										write('Val '),write(ValRao),write(' W '),write(WRao),nl,
										NewP is Pval*WRao
									    ),NewDistr),
							nl,
							findall(AA,(recorded(I,AA,_),write(AA),nl),_),
							nl,
							write('NewDistr '),write(NewDistr),nl,
							sum_prob(NewDistr,Sum),
							divideby(NewDistr,Sum,DistrNorm),
							write('W '),write(Sum),nl,
							write('DistrNorm '),write(DistrNorm),nl,
							erase(R),
							cleanDistribution(DistrNorm,Cleaned,0.0),
							recorda(I,next(VarRao) ~= finite(Cleaned),_)
						),ListSum),
					write('ListSum '),write(ListSum),nl,				
					product_list(ListSum,P1),
					write('------------------'),
%					P=Sum
					% for kalman filter  to complete!!!!!!!
					bb_put(pkalman,1.0),
					(% evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...)
						% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
						user:rao(VarRao),
						user:distributionalclause(next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_),
						%trace,
						proof_query_backward(I,Body), %query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
						ground(VarRao),
						ground(kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas)),
						
						%write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
						recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
						% if not recorded find the prior:
						% TO DO!!
						%
						(
							member(observation(VarRao)~=Vevidence,PosEvidence) ->
							true
							;
							(Vevidence=[])
						),
						write(kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman)),nl,
						
						kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman),
						recorda(I,next(VarRao) ~= gaussian(Mpost,CovPost),_),
						
						bb_get(pkalman,PKalman),
						NewPKalman is PKalman*Wkalman,
						bb_put(pkalman,NewPKalman),
						write('Wkalman '),write(Wkalman),nl,
						fail;
						true
					
					),
					(
					
						user:rao(VarRao),
						user:distributionalclause(next(VarRao),gaussian(M,Cov),Body,_),
						%trace,
						proof_query_backward(I,Body), %query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
						ground(VarRao),
						ground(gaussian(M,Cov)),
						recorda(I,next(VarRao) ~= gaussian(M,Cov),_),
						%write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
						%recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
						% if not recorded find the prior:
						% TO DO!!
						%
						member(observation(VarRao)~=Vevidence,PosEvidence),
						
						%write(kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman)),nl,
						
						%kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman),
						
						densityGaussian(M,Cov,Vevidence,Wkalman),
						bb_get(pkalman,PKalman),
						NewPKalman is PKalman*Wkalman,
						bb_put(pkalman,NewPKalman),
						write('Wkalman '),write(Wkalman),nl,
						fail;
						true
					
					),
					bb_delete(pkalman,PtotKalman),
					P is P1*PtotKalman
				)
				;
				(		
					get_magic(false) ->
					(
						eval_weight_magicoff(I,PosEvidence,P)
					)
					;
					true
					
				)
			)
		),
		dcpf:bb_get(I,OldWeight),
		NewW is OldWeight+P,
		dcpf:bb_put(I,NewW),
		
		dcpf:bb_get(likelihood,Likelihood),
		NewLikelihood is Likelihood+exp(NewW),
		dcpf:bb_put(likelihood,NewLikelihood),
		fail;
		true
	),
	dcpf:bb_get(likelihood,TotLikelihood),
	dcpf:bb_get(loglikelihood,LogLikelihood),
	NewLogLikelihood is LogLikelihood+log(TotLikelihood/N),
	dcpf:bb_put(loglikelihood,NewLogLikelihood),
	(
		user:lifted(true) ->
		(
			(distributionalclause:proof_query_backward_lifted(global,next(A)),fail;true),
			distributionalclause:current2next(global)
		)
		;
		true
	),
	dcpf:bb_put(initdcpf,false).

step_particle4(Info,NumberRecognized,N) :-
	
	Delta is 1.0,
	%statistics(cputime,[TimeInit,_]),
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	dcpf:bb_get(offset,Offset),
	dcpf:bb_put(likelihood,0.0),
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	%write('timestep '),write(T),nl,
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		dcpf:abolish_all_tables,
		eraseall(tempparticle),
		findall(O,(distributionalclause:proof_query_backward(I,tempparticle,(current(on(O,table))~=true))),PreviousOnTable),
		
		%nl,
		%writeln(I),
		%writeln(List),
		%writeln(pre_step:PreviousOnTable),
		objectsObservationIteration2(PreviousOnTable,NumberRecognized,Info,I,ObsT,PosEvidence,Actions),
		%writeln(PosEvidence),
		assert_list(I,PosEvidence),
		assert_list(I,ObsT), 
		assert_list(I,Actions),
		dcpf:bb_get(I,LogTempWeight),
		TempWeight is exp(LogTempWeight),
		TempWeight>0.0,
		dcpf:bb_get(initdcpf,InitDCPF),
		(
			get_magic(true) ->
			(
				init_query(I,next(_)),
				init_query(I,next(_) ~= _),
				init_query_list(I,PosEvidence)
			)
			;
			true
		),
		
		(
			get_magic(backward) ->
			(
				user:raoblackwellisation(false) ->
				(	
					dcpf:logweight_constraints(10,I,PosEvidence,Constraints,P)
				)
				;
					true
			)
			;
			true
		),
		(
			(InitDCPF==false) ->
			(
				get_magic(backward) ->
					dcpf:inferencestep_particlefilter_backward3(I)
				;
					dcpf:generate_sample_particlefilter(I,MaxP)
			)
			;
			true
		),
		
		(
			user:raoblackwellisation(true) ->
			(
				add_rao_backward(I,PosEvidence),
				inferencestep_particlefilter_magicoff_rao(I,_),
				(
					get_magic(backward) ->
						inferencestep_particlefilter_backward2(I)
					;
						generate_sample_particlefilter(I,MaxP)
				)
			)
			;
				true
		),
		
		(
			get_magic(true) ->
				eval_weight(I,PosEvidence,P)
			;
			(
				user:raoblackwellisation(true) ->
				(
					nl,nl,write('particle '),write(I),nl,
					findall(Sum,
						(
							recorded(I,next(VarRao) ~= finite(Distr),R),
							user:rao(VarRao),
							
							write(next(VarRao) ~= finite(Distr)),nl,
							findall(NewP:ValRao,(
										member(Pval:ValRao,Distr),
										
										eval_weightRao(I,PosEvidence,WRao,VarRao,ValRao),
										write(eval_weightR(I,PosEvidence,WRao,VarRao,ValRao)),nl,
										write('Val '),write(ValRao),write(' W '),write(WRao),nl,
										NewP is Pval*WRao
									    ),NewDistr),
							nl,
							findall(AA,(recorded(I,AA,_),write(AA),nl),_),
							nl,
							write('NewDistr '),write(NewDistr),nl,
							sum_prob(NewDistr,Sum),
							divideby(NewDistr,Sum,DistrNorm),
							write('W '),write(Sum),nl,
							write('DistrNorm '),write(DistrNorm),nl,
							erase(R),
							cleanDistribution(DistrNorm,Cleaned,0.0),
							recorda(I,next(VarRao) ~= finite(Cleaned),_)
						),ListSum),
					write('ListSum '),write(ListSum),nl,				
					product_list(ListSum,P1),
					write('------------------'),
%					P=Sum
					% for kalman filter  to complete!!!!!!!
					bb_put(pkalman,1.0),
					(% evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...)
						% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
						user:rao(VarRao),
						user:distributionalclause(next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_),
						%trace,
						proof_query_backward(I,Body), %query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
						ground(VarRao),
						ground(kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas)),
						
						%write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
						recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
						% if not recorded find the prior:
						% TO DO!!
						%
						(
							member(observation(VarRao)~=Vevidence,PosEvidence) ->
							true
							;
							(Vevidence=[])
						),
						write(kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman)),nl,
						
						kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman),
						recorda(I,next(VarRao) ~= gaussian(Mpost,CovPost),_),
						
						bb_get(pkalman,PKalman),
						NewPKalman is PKalman*Wkalman,
						bb_put(pkalman,NewPKalman),
						write('Wkalman '),write(Wkalman),nl,
						fail;
						true
					
					),
					(
					
						user:rao(VarRao),
						user:distributionalclause(next(VarRao),gaussian(M,Cov),Body,_),
						%trace,
						proof_query_backward(I,Body), %query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
						ground(VarRao),
						ground(gaussian(M,Cov)),
						recorda(I,next(VarRao) ~= gaussian(M,Cov),_),
						%write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
						%recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
						% if not recorded find the prior:
						% TO DO!!
						%
						member(observation(VarRao)~=Vevidence,PosEvidence),
						
						%write(kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman)),nl,
						
						%kalmanrao(M,Cov,A,B,MeanSys,CovSys,H,MeanMeas,CovMeas,Input,Vevidence,Mpost,CovPost,Wkalman),
						
						densityGaussian(M,Cov,Vevidence,Wkalman),
						bb_get(pkalman,PKalman),
						NewPKalman is PKalman*Wkalman,
						bb_put(pkalman,NewPKalman),
						write('Wkalman '),write(Wkalman),nl,
						fail;
						true
					
					),
					bb_delete(pkalman,PtotKalman),
					P is P1*PtotKalman
				)
				;
				(		
					get_magic(false) ->
					(
						eval_weight_magicoff(I,PosEvidence,P)
					)
					;
					true
					
				)
			)
		),
		dcpf:bb_get(I,OldWeight),
		NewW is OldWeight+P,
		dcpf:bb_put(I,NewW),
		
		dcpf:bb_get(likelihood,Likelihood),
		NewLikelihood is Likelihood+exp(NewW),
		dcpf:bb_put(likelihood,NewLikelihood),
		fail;
		true
	),
	dcpf:bb_get(likelihood,TotLikelihood),
	dcpf:bb_get(loglikelihood,LogLikelihood),
	NewLogLikelihood is LogLikelihood+log(TotLikelihood/N),
	dcpf:bb_put(loglikelihood,NewLogLikelihood),
	(
		user:lifted(true) ->
		(
			(distributionalclause:proof_query_backward_lifted(global,next(A)),fail;true),
			distributionalclause:current2next(global)
		)
		;
		true
	),
	dcpf:bb_put(initdcpf,false).

%TODO normaal weg
/*
objectsObservationIterationParticles2(I,NO2,Info,Offset,N) :-
	I > N.
objectsObservationIterationParticles2(I,NO2,Info,Offset,N) :-
	eraseall(tempparticle),
	findall(O,(distributionalclause:proof_query_backward(I,tempparticle,(current(on(O,table))~=true))),List),
	nl,
	writeln(particle:I2),
	objectsObservationIteration2(List,NO2,Info,I2,ObsT),
	writeln(obst:ObsT),
	I3 is I+1,
	objectsObservationIterationParticles2(I3,NO2,Info,Offset,N).
*/

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
