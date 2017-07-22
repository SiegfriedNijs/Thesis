%%% -*- Mode: Prolog; -*-

:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module('utilities.pl').
:- use_module('knowledge_base.pl').
:- use_module('ontology.pl').

:- dynamic timepoint/1.

builtin(get_instances(_,_)).

most_informative_question(N) :-
	writeln('---------------------------'),	
	eval_query_particle2((B2e),current(wanted_action(He))~=B2e,N,R2e),
	writeln(R2e),
	typeQuestion(R2e,ValueT),
	objectQuestion(R2e,ValueO),
	gripperQuestion(R2e,ValueGr),
	Values = [ValueT,ValueO,ValueGr],
	writeln(ValueT),
	writeln(ValueO),
	writeln(ValueGr),
	Questions = [type,object,gripper],
	max_list(Values,Chosen),
	get_index(Chosen,Values,Index),
	nth1(Index,Questions,ChosenQ),
	writeln(ChosenQ),
	askQuestion(ChosenQ,R2e,N).

askQuestion(type,Distr,N) :-
	write('Which of these actions do you need? '),
	findall(Type,(member((_:(Type,_,_)),Distr)),Types1),
	remove_duplicates(Types1,Types),
	writeln(Types).

askQuestion(object,Distr,N) :-
	write('Which of these objects did you mean?'),
	findall(O,(member((_:(_,O,_)),Distr)),Os1),
	remove_duplicates(Os1,Os),
	unique_object_descriptions(Os, DD),
	append(Os,[nocare],Os2),
	writeln(DD),
	writeln(Os2).
	
askQuestion(tool,Distr,N) :-
	nl,	
	writeln(---------------------------------------),
	S1 = 'Which of these tools do I need to use? ',
	write(S1),
	findall(Gr,(member((_:(_,_,Gr)),Distr)),Grs1),
	remove_duplicates(Grs1,Grs),
	append(Grs,[nocare],Grs2),
	writeln(Grs2),
	writeln(----------------------------------------).

give_answer(Answer,type,N) :-
	Command = [current(wanted_action(human(1))) ~= (Answer,_,_)],
	commandObservation(human(1),Command,N).%TODO comm nummer

give_answer(Answer,object,N) :-
	Command = [current(wanted_action(human(1))) ~= (_,Answer,_)],
	commandObservation(human(1),Command,N).%TODO comm nummer
%TODO nocare variant

give_answer(Answer,gripper,N) :-
	Command = [current(wanted_action(human(1))) ~= (_,_,Answer)],
	commandObservation(human(1),Command,N).%TODO comm nummer
%TODO nocare variant

typeQuestion(WADistrib,ValueT) :-
	findall(Type,(member((_:(Type,_,_)),WADistrib)),Types),
	remove_duplicates(Types,Types2),
	questionValue(type,WADistrib,WADistrib,Types2,ValueT).

objectQuestion(WADistrib1,ValueO) :-
	findall(O,(member((_:(_,O,_)),WADistrib1)),Os),
	remove_duplicates(Os,Os2),
	writeln(Os2),
	append(Os2,[nocare],Os3),
	writeln(Os3),
	Care = 0.9,%TODO afh van situatie
	CareDiff is 1 - Care,
	writeln(Care),
	writeln(CareDiff),
	processDistribution(WADistrib1,Care,WADistrib2),
	writeln(WADistrib2),
	append(WADistrib2,[CareDiff:(_,nocare,_)],WADistrib3),
	writeln(WADistrib3),
	questionValue(object,WADistrib1,WADistrib2,Os3,ValueO).

gripperQuestion(WADistrib1,ValueGr) :-
	findall(Gr,(member((_:(_,_,Gr)),WADistrib1)),Grs),
	remove_duplicates(Grs,Grs2),
	writeln(Grs2),
	append(Grs2,[nocare],Grs3),
	writeln(Grs3),
	Care = 0.1,
	CareDiff is 1 - Care,
	writeln(Care),
	writeln(CareDiff),
	processDistribution(WADistrib1,Care,WADistrib2),
	writeln(WADistrib2),
	append(WADistrib2,[CareDiff:(_,nocare,_)],WADistrib3),
	writeln(WADistrib3),
	questionValue(gripper,WADistrib1,WADistrib2,Grs2,ValueGr).


questionValue(_,_,_,[],0).
questionValue(TT,WADistrib1,WADistrib2,[nocare|TR],Value) :-
	questionValue(TT,WADistrib1,WADistrib2,TR,Value).
questionValue(TT,WADistrib1,WADistrib2,[T1|TR],Value) :-
	answerProb(TT,T1,WADistrib1,AProb1),
	answerProb(TT,T1,WADistrib2,AProb2),
	InfV is log(1/AProb1)/log(2),
	V1 is AProb2*InfV,
	questionValue(TT,WADistrib1,WADistrib2,TR,VR),
	Value is V1 + VR.

answerProb(type,T1,WADistrib,AProb) :-
	findall(Prob,(member((Prob:(T1,_,_)),WADistrib)),ProbList),
	sum_list(ProbList,AProb).

answerProb(object,O,WADistrib,AProb) :-
	findall(Prob,(member((Prob:(_,O,_)),WADistrib)),ProbList),
	sum_list(ProbList,AProb).

answerProb(gripper,Gr,WADistrib,AProb) :-
	findall(Prob,(member((Prob:(_,_,Gr)),WADistrib)),ProbList),
	sum_list(ProbList,AProb).	

processDistribution([],_,[]).
processDistribution([Prob1:Info|ProbR],CareDiff,[Prob2:Info|ProbR2]) :-
	Prob2 is Prob1*CareDiff,
	processDistribution(ProbR,CareDiff,ProbR2).

get_predicted_classes(Object,PredCls) :-
	(has_category(Object) ~ finite(L) := true),
	get_predicted_classes2(L,PredCls).
get_predicted_classes2([],[]).
get_predicted_classes2([H|R],[H2|R2]) :-
	H = A:H2,
	get_predicted_classes2(R,R2).

get_variables([],[]).
get_variables([H|R],[H2|R2]) :-
	get_variables2(H,H2),
	get_variables(R,R2).
get_variables2(Clause, Vars) :-
	term_variables(Clause,Vars).

get_domains([],[],[]).
get_domains([Qh|Qrest],[Lh|Lrest],[D2|D]) :-
	get_domains2(Qh,Lh,D2),
	get_domains(Qrest,Lrest,D).
get_domains2(_,[],[]).
get_domains2(Qh,[Lhh|Lhr],[D2h|D2r]) :-
	get_domains3(Qh,Lhh,D2h),
	get_domains2(Qh,Lhr,D2r).
get_domains3(distributionalclause(Head, D,B,E), Var, Domain) :-
	distributionalclause:eval_query_backward_exp([],[],(domain(Var,X)),1000,Prob,_,_).

compensation_action([Name1,pickup,[O,X,Y,Z,Gripper]],(Name,place,[O,X,Y,Z,Gripper]),N) :-
	eval_query_particle2((B),current(wanted_effect(Name1))~=B,N,R),
	R = [1.0:true], %NP maak prob.
	timestep(Time),
	get_perfname(Time,Name).
compensation_action([Name1,pickup,[O,X,Y,Z,Gripper]],(none,none,[n,n,n,n,n]),N) :-
	eval_query_particle2((B),current(wanted_effect(Name1))~=B,N,R),
	R = [1.0:false]. %NP maak prob. beslissing
compensation_action([Name1,place,[O,X,Y,Z,Gripper]],(Name,pickup,[O,X,Y,Z,Gripper]),N) :-
	timestep(Time),
	get_perfname(Time,Name).
compensation_action([Name1,info,[_,_,_,_,_]],(none,none,[n,n,n,n,n]),N).

get_instances(time_ca,[T]) :-
	%writeln('get instances time'),
	timepoint(T).

/*
Gets all the instances that have a chance > 0 to belong to the given class.
*/
%eval_query_particle and 2 
get_instances(Class,T,Ins) :-
	writeln('get instances'), writeln(Class),
	findall(X,is_subcategory(X,Class),L),
	append(L,[Class],Lextra),
	writeln(ins_klassen:Lextra),
	findall(X1, instance2(X,T)~=true,L3), %%TODO beter dan met een kans
	%distributionalclause:eval_query_backward_exp([],[],(has_category(Y,X1,T)~=true),10,Prob,_,_), Prob>0) %%(member(Y,Lextra),
	remove_duplicates(L3,L4),
	Ins = L4,!,
	writeln(L4).

certain_not_holding(Arm,N) :-
	eval_query_particle2(H,current(holding_object(Arm))~=H , N , P1),
	certain(P1,none).

extra_parameters([pickup_ca,Object],P,N) :-
	(certain_not_holding(left_gripper,N) ->
		writeln('not holding anything in left gripper'),
		P = [left_gripper]
	;
		(certain_not_holding(right_gripper,N) ->
			writeln('not holding anything in right gripper'),
			P = [right_gripper]
		;
			writeln('holding in both grippers')	
			%TODO
		)
	).

get_action(T,A) :-
	atomic_concat([action,T],A).	

combine_description([],[],[]).
combine_description([H1|R1],[H2|R2],[H3|R3]) :-
	H3 = [H1,H2],
	combine_description(R1,R2,R3).

unique_object_descriptions(Objs, DD) :-
	get_couples(Objs,Couples),
	single_object_categories(Objs,D1),
	single_object_colours(Objs,D2),
	combine_description(D1,D2,Dres),
	prune_clarities(cat,Couples,Objs,D2,C2),
	prune_clarities(col,C2,Objs,D2,C3),
	get_spatial_differences(C2,SpatDiff),
	get_spatial_descriptions(Objs,Objs,Dres,C3,SpatDiff,DD).

get_couples(L,R) :-
	findall([O1,O2], (member(O1,L),member(O2,L),O1 \= O2), R).

get_spatial_descriptions([],_,_,_,_,[]).
get_spatial_descriptions([O|R],Objs,D1,Uncl,SpatDiff,[H2|R2]) :-
	( member([O,O2],Uncl) ->
		add_spatial_comparisons(O,Objs,Uncl,D1,SpatDiff,FinalDescription),
		H22 = FinalDescription,
		flatten(H22,H2)
	;
		nth1(I1,Objs,O),
		nth1(I1,D1,Desc1),
		H2 = Desc1
	),
	get_spatial_descriptions(R,Objs,D1,Uncl,SpatDiff,R2).


add_spatial_comparisons(O,Objs,Uncl,D1,SpatDiff,FinalDescription) :-
	nth1(I1,Objs,O),
	nth1(I1,D1,Desc1),
	findall(O2,(member([O,O2],Uncl)),Os),
	%writeln(Is),
	add_spatial_comparisons2(O,Objs,Os,SpatDiff,Uncl,D1,SpatDs),
	FinalDescription = [Desc1|SpatDs].

add_spatial_comparisons2(_,_,[],_,_,_,[]).
add_spatial_comparisons2(O,Objs,[O2|R],SpatDiff,Uncl,D1,[SD|R2]) :-
	nth1(I,Uncl,[O,O2]),
	nth1(I,SpatDiff,Diff),
	nth1(I2,Objs,O2),
	nth1(I2,D1,Desc2),
	SD = [Diff,Desc2],
	add_spatial_comparisons2(O,Objs,R,SpatDiff,Uncl,D1,R2).

get_spatial_differences([],[]).
get_spatial_differences([H|R],[SpatDiff|R2]) :-
	%TODO neem aan 2 objecten voorlopig
	%eventueel between nog
	get_spatial_differences2(H,SpatDiff),%TODO neem aan altijd een difference
	get_spatial_differences(R,R2).

get_spatial_differences2(H,leftof) :-
	nth1(1,H,O1),
	nth1(2,H,O2),
	eval_query_particle2(LO,current(leftof(O1,O2))~=LO, 200, P), %np 200
	writeln(P),
	certain(P,true).
get_spatial_differences2(H,rightof) :-
	nth1(1,H,O1),
	nth1(2,H,O2),
	eval_query_particle2(RO,current(rightof(O1,O2))~=RO, 200, P),
	certain(P,true).
get_spatial_differences2(H,frontof) :-
	nth1(1,H,O1),
	nth1(2,H,O2),
	eval_query_particle2(RO,current(frontof(O1,O2))~=RO, 200, P),
	certain(P,true).
get_spatial_differences2(H,backof) :-
	nth1(1,H,O1),
	nth1(2,H,O2),
	eval_query_particle2(RO,current(backof(O1,O2))~=RO, 200, P),
	certain(P,true).

prune_clarities(cat,[],_,_,[]).
prune_clarities(cat,[[object(O1),object(O2)]|Crest],Objs,D1,Return) :-
	get_index(object(O1),Objs,I1),
	get_index(object(O2),Objs,I2),
	nth1(I1,D1,Cat1),
	nth1(I2,D1,Cat2),
	(unclear_distinction(cat,Cat1,Cat2) ->
		Return = [[object(O1),object(O2)]|R2]
		
	;
		Return = R2
	),
	prune_clarities(cat,Crest,Objs,D1,R2).

prune_clarities(col,[],_,_,[]).
prune_clarities(col,[[object(O1),object(O2)]|Crest],Objs,D2,Return) :-
	get_index(object(O1),Objs,I1),
	get_index(object(O2),Objs,I2),
	nth1(I1,D2,C1),
	nth1(I2,D2,C2),
	(unclear_distinction(col,C1,C2) ->
		Return = [[object(O1),object(O2)]|R2]
		
	;
		Return = R2
	),
	prune_clarities(col,Crest,Objs,D2,R2).

unclear_distinction(cat,Cat1,Cat2) :-
	is_subcategory(Cat1,Cat2).
unclear_distinction(cat,Cat1,Cat2) :-
	is_subcategory(Cat2,Cat1).
unclear_distinction(cat,Cat1,Cat1).

unclear_distinction(col,unknown,Col2).
unclear_distinction(col,Col1,unknown).
unclear_distinction(col,Col1,Col2) :-
	Col1 == Col2.


get_unclarities(Objs,Ds,Us1) :-
	length(Objs,Length),
	get_unclarities2(1,Length,Objs,Ds,Us),
	remove_duplicates(Us,Us1).

get_unclarities2(N,Length,Objs,Ds,Return) :-
  	N =< Length,
	nth1(N,Ds,Descr),
	findall(Ns,(nth1(Ns,Ds,Descr)),Numbers),
	findall(O, (member(Nu,Numbers),nth1(Nu,Objs,O)),Objs2),
	length(Objs2,Length2),	
	(Length2 >1 ->
		Return = [Objs2|R2]
	;
		Return = R2
	),
	N2 is N+1,
	get_unclarities2(N2,Length,Objs,Ds,R2).
get_unclarities2(_,_,_,_,[]).

single_object_categories([],[]).
single_object_categories([O1|R],[D|R2]) :-
	certain_lowest_subcategories(O1,Cats),
	nth1(1,Cats,Cat1),%TODO neem 1ste
	D = Cat1,
	single_object_categories(R,R2).

single_object_colours([],[]).
single_object_colours([O1|R],[D|R2]) :-
	%certain_colour(basic,O1,Col),
	certain_colour(extended,O1,Col),
	(Col == [] ->
		D = unknown %TODO verschil maken met uncertain??
	;
		D = Col
	),
	single_object_colours(R,R2).

certain_colour(basic,O1,Col) :-
	has_physical_generalRGB(O1,RGB),
	writeln(RGB),
	closestBasicColour(RGB,Col).
certain_colour(extended,O1,Col) :-
	has_physical_generalRGB(O1,RGB),
	writeln(RGB),
	closestExtColour(RGB,Col).

get_common_category(Is,CommonC) :-
	get_common_category2(Is,LowestSubs),
	%write('Individual lowest certain category: '),writeln(LowestSubs),
	get_common_category3(LowestSubs,[anything_ca],Commons),
	nth1(1,Commons,CommonC).
get_common_category2([],[]).
get_common_category2([H|R],[H2|R2]) :-
	certain_lowest_subcategories(H,Subs),
	nth1(1,Subs,H2),
	get_common_category2(R,R2).
get_common_category3(Cats,L,Commons) :-
	findall(X,(member(Y,L), subcategory(X,Y)),R),
	prune_notcommon(Cats,R,R2),
	length(R2,Le),
	(Le>0 ->
		get_common_category3(Cats,R2,Commons)
	;
		Commons = L
	).
		
prune_notcommon(Cats,[],[]).
prune_notcommon(Cats,[H|R],Return) :-
	(are_subcategories(Cats,H) ->
		Return = [H|R2]
	;
		Return = R2
	),
	prune_notcommon(Cats,R,R2).

are_subcategories([],_).	
are_subcategories([C1|CR],H) :-
	is_subcategory(C1,H),
	are_subcategories(CR,H).

certain_lowest_subcategories(I,L) :-
	certain_lowest_subcategories2(I,[anything_ca],L).
certain_lowest_subcategories2(I,L,L2) :-
	findall(X,(member(Y,L), subcategory(X,Y)),R),
	prune_uncertain_categories(I,R,R2),
	length(R2,Length2),
	( Length2 > 0 ->
		certain_lowest_subcategories2(I,R2,R3),
		length(R3,Length3),
		(Length3 >0 ->
			L2 = R3
		;
			L2 = R2
		)	
	;
		L2 = []
	).

certain(Distr,Val) :-
	Distr = [1.0:Val].

prune_uncertain_categories(_,[],[]).
prune_uncertain_categories(I,[H|R],Return) :-
	eval_query_particle2(P,current(has_category(H,I))~=P, 200, P1),%TODO 200 overal weg
	( certain(P1,true) ->
		Return = [H|R2]
	;
		Return = R2
	),
	prune_uncertain_categories(I,R,R2).
	

/*
Returns most probable human from info.
%TODO extend
*/
getHuman(distributionalclause(commandgiver,finite([1:Human]),true,1), Human).

labeling([]).
labeling([distributionalclause(Head,_,_,_)|R]) :-
	term_variables(Head,Vars),%nl,nl,
	%writeln(Head),writeln(Vars),nl,nl,
	labeling2(Head, Vars),
	labeling(R).
labeling2(_,[]) :- true. % writeln(end_labeling).
labeling2(Clause,[H|R]) :-
	functor(Clause,Name,N),
	get_var_index(Clause,H,Nh,N),
	functor(Clause2,Name,N),
	arg(Nh,Clause2,H2),	
	findall(H2,Clause2,L),
	nth1(1,L,El),
	get_instances(El,Ins),
	!,
	member(H,Ins),
	labeling2(Clause,R).
	
get_var_w_domain(distributionalclause(Head,_,_,_), Domain, Var) :-
	writeln(Head),
	functor(Head,Name,N),
	get_var_w_domain2(Head, Domain, N, Var).
	
get_var_w_domain2(Clause, Domain, 0, Var).
get_var_w_domain2(Clause, Domain, N, Var) :-
	arg(N,Clause,Arg),
	functor(Clause,Name,Ntot),
	functor(Clause2,Name,Ntot),
	arg(N,Clause2,D),
	findall(D,Clause2,L),
	nth1(1,L,El),
	writeln(El),
	El == Domain,
	Var = Arg.
get_var_w_domain2(Clause, Domain, N, Var) :-
	N2 is N-1,
	get_var_w_domain2(Clause, Domain, N2, Var).

find_object_differences(Cat, Objs, Diffs) :-
	findall(P,
	(member(O,Objs), distributionalclause:eval_query_backward_distrib( [] , [] ,(O,Prop) , (has_object_property(O)~=Prop) , 100 , P)),
	R),
	flatten(R,R2),
	findall( (Obj,Property) ,(member((Prob2:(Obj,Property)),R2)),R3),	
	associate_properties(Objs,R3,Diffs).

associate_properties([],_,[]).
associate_properties([H|R],L,[L2|Re]) :-
	findall(Prop,member((H,Prop),L),L2),
	associate_properties(R,L,Re).

can_be(I,Class) :-
	get_instances(Class,Ins),
	memberchk(I,Ins).

get_var_index(Clause,H,0,0).
get_var_index(Clause,H,N2,N) :-
	arg(N,Clause,H2),
	H2 == H,
	N2 is N.
get_var_index(Clause,H,N2,N) :-
	N3 is N-1,
	get_var_index(Clause,H,N2,N3).


convert_to_query([],[]).
convert_to_query([H|R],[H2|R2]) :-
	H = distributionalclause(Head,finite([1:Value]),_,_),
	H2 = ~=(Head,Value),
	convert_to_query(R,R2).

/*
Spatial knowledge processing.
*/

overlapping([I1min,I1max],[I2min,I2max]) :-
	I1max >= I2min,
	I1min =< I2max.

distance_interval([I1min,I1max],[I2min,I2max],D) :-
	overlapping([I1min,I1max],[I2min,I2max]),
	D is 0.
distance_interval([I1min,I1max],[I2min,I2max],D) :-
	I1max < I2min,
	D is I2min - I1max.
distance_interval([I1min,I1max],[I2min,I2max],D) :-
	I1min > I2max,
	D is I1min - I2max.



/*
near(O2,O1,T)~ finite([1:true]) :=
	holds_content(O1,C1) ~=true,	
	near(C1,O2,T)~=true.
	%writeln(teest).
near(O1,O2,T)~ finite([1:true]) :=
	holds_content(O2,C2) ~=true,	
	near(O1,C2,T)~=true.
near(O1,O2,T)~ finite([1:true]) :=
	holds_content(O1,C1) ~=true,
	holds_content(O2,C2) ~=true,	
	near(C1,C2,T)~=true.
*/



%TODO frontof, backof, anderen?


/*
on(Cont,Surf,T) ~ finite([1:true]) :=
		holds_content(O1,Cont) ~=true,
		on(O1,Surf,T)~=true.
on(Cont,Surf,T) ~ finite([1:false]) :=
		holds_content(O1,Cont) ~=true,
		on(O1,Surf,T)~=false.
*/

/*
Probability processing.
*/

/*
Acceptance parameter = 0 => Everything above average prob is accepted, below is rejected.
Acceptance parameter = 1 => Everything is accepted, nothing pruned.
%TODO small test with more than  2 probs
*/
probability_processing(P4, AcceptParam, Pruned) :- 
	%writeln(P4),
	probabilities_sum(P4,Sum),
	%writeln(Sum),
	relative_probs(P4,Sum,RelProbs),
	%writeln(RelProbs),
	length(P4,Length),
	Tresh is (1/Length) - (AcceptParam/Length),
	prune_probabilities(RelProbs,Tresh,Pruned).
	%writeln(RelProbs),
	%writeln(Tresh),
	%writeln(Pruned).

prune_probabilities([],_,[]).
prune_probabilities([(H:X)|RelProbs],Tresh,R) :-
	( H>Tresh ->
		prune_probabilities(RelProbs,Tresh,Pruned),
		R = [(H:X)|Pruned]
	;
		prune_probabilities(RelProbs,Tresh,Pruned),
		R = Pruned
	).	

probabilities_sum([],0).
probabilities_sum([(Pr:_)|R],Sum2) :-
	probabilities_sum(R,Sum),
	Sum2 is Sum + Pr.

relative_probs([],_,[]).
relative_probs([(H:X)|R],Sum,[(H2:X)|R2]) :-
	H2 is H/Sum,
	relative_probs(R,Sum,R2).


