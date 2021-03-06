%%% -*- Mode: Prolog; -*-

:- use_module(library(lists)).
:- use_module(library(aggregate)).
:- use_module('../distributionalclause.pl').
:- use_module('../random/sampling.pl').
:- use_module('utilities.pl').
:- use_module('knowledge_base.pl').

set_algorithm(argmax).
%set_algorithm(tree).

%calculates best action based on a pomdp approach
get_best_waction(BestAction,N) :-
	set_algorithm(tree),
	Human = human(1),%NP
	eval_query_particle2((AT,[F,X,Y,Z,Params]),(current(wanted_action(Human))~=(AT,F,Params),current(has_position(F))~=(X,Y,Z)),N,P1),%NP action terminologie inc.
	eval_query_particle2([C1,C2,C3],(current(care(Human,type))~=C1,current(care(Human,object))~=C2,current(care(Human,tool))~=C3),N,P2),	
	possibleActionsLoop(P1,P2,Actions),	
	addCosts(Actions,Actions2),
	get_probs(Actions2,Probs),
	min_list(Probs,Best),
	member((Best:BestAction1),Actions2),
	timestep(Time),
	get_perfname(Time,Na),
	BestAction1 = (At,Pr),
	BestAction = (Na,At,Pr),
	nl,write('Selected best action is: '),writeln(BestAction),
	add_new_evidence([perf(Na,At,Pr)]).

%main loop of the pomdp recursive generation process, can be built to certain level
possibleActionsLoop(_,[],[]).
possibleActionsLoop(Pn,[C1|R1],Res) :-
	C1 = (Prob:C11),
	possibleActions(Pn,C11,Actions,2),%last argument indicates to which level to build
	weighActions(Prob,Actions,Actions2),
	possibleActionsLoop(Pn,R1,R2),
	append(Actions2,R2,Res).

%with current knowledge on 3 parameter user cares and the current WA distribution, the possible actions are calculated that can help clear up the uncertainties further; these methods also include the bottom up transitive process of bringing the costs to the top of the pomdp
possibleActions(_,_,[],0).
possibleActions([],_,[],_).
possibleActions([(_:(AT,[F,X,Y,Z,Params]))],_,[E],Level) :-
	costAction(AT,ACost),
	costAction(posfeedback,CostFeedb),
	FinalC is ACost + CostFeedb,
	E = (FinalC:(AT,[F,X,Y,Z,Params])).
possibleActions(Pn,Care,Actions,Level) :-
	possibleExecutions(Pn,Pn,Care,Executions,Level),
	possibleQuestions(Pn,Pn,Care,Questions,Level),
	append3(Executions,Questions,Actions).
append3(Executions,Questions,Actions) :-
	append(Executions,Questions,Actions).

%calculation of the current possible action plan executions, bottom up calculation of the cost of each branch
possibleExecutions([],_,_,[],_).
possibleExecutions([H],Pn,Care,[E],_) :-
	H = (Prob:(AT,[F,X,Y,Z,Params])),
	costAction(AT,ACost),
	costAction(posfeedback,CostFeedb),
	FinalC is ACost + CostFeedb,
	E = (FinalC:(AT,[F,X,Y,Z,Params])).
possibleExecutions([H|R],Pn,Care,[E|RE],Level) :-
	H = (Prob:(AT,[F,X,Y,Z,Params])),
	List = [AT,F,Params],
	getCareFormula(List,Care,Formula),
	Formula = [AT2,F2,Params2],
	findall(Prob,(member((_:(AT2,[F2,_,_,_,Params2])),Pn)),LL),
	sum_list(LL,SUM),
	costAction(AT,ACost),
	costAction(posfeedback,CostFeedb),
	CostOK is SUM*(0+CostFeedb),
	reverseAction(AT,RT),
	costAction(RT,RCost),	
	processDistrActionFeed(Pn,Care,H,Pn2),	
	getMinCost(Pn2,Care,MinC,Level),
	CostNOK is (1-SUM)*(RCost + MinC),
	FinalC is ACost + CostOK + CostNOK,
	E = (FinalC:(AT,[F,X,Y,Z,Params])),
	possibleExecutions(R,Pn,Care,RE,Level).

%calculation of the current possible questions to ask, bottom up calculation of the cost of each branch
possibleQuestions([],Pn,Care,[],Level).
possibleQuestions(Pn,Pn,Care,[],Level) :-
	length(Pn,Le),
	Le == 1.
possibleQuestions(Pn,Pn,Care,Questions2,Level) :-
	typeQuestionBranch(Pn,Care,TC,Level),%!,
	objectQuestionBranch(Pn,Care,OC,Level),%!,
	toolQuestionBranch(Pn,Care,ToolC,Level),%!,
	Questions = [(TC:(typeq,[object(100),0,0,0,robotbrain])),(OC:(objectq,[object(100),0,0,0,robotbrain])),(ToolC:(toolq,[object(100),0,0,0,robotbrain]))],
	pruneQuestion(Questions,Care,Questions2). 
%NP extra questions

%pruning of questions that have no use given the care knowledge; if a user does not care about a choice then don't present the choice
pruneQuestion([],[],[]).
pruneQuestion([(10000:_)|QR],[C|CR],QR2) :-
	pruneQuestion(QR,CR,QR2).
pruneQuestion([Q|QR],[C|CR],[Q|QR2]) :-
	C = care,
	pruneQuestion(QR,CR,QR2).
pruneQuestion([Q|QR],[C|CR],QR2) :-
	C = nocare,
	pruneQuestion(QR,CR,QR2).

%question branches, with calculation of expected cost and future states
objectQuestionBranch(Pn,Care,TC,Level) :-
	nth1(2,Care,nocare),
	TC = 10000.
objectQuestionBranch(Pn,Care,TC,Level) :-
	length(Pn,Le),
	Le==1,
	TC = 10000.
objectQuestionBranch(Pn,Care,TC,Level) :-
	nth1(2,Care,care),
	findall(O,(member((_:(_,[O,_,_,_,_])),Pn)),Os),
	remove_duplicates(Os,Os2),
	length(Os2,Le),
	Le==1,
	TC = 10000.
objectQuestionBranch(Pn,Care,TC,Level) :-
	nth1(2,Care,care),
	findall(O,(member((_:(_,[O,_,_,_,_])),Pn)),Os),
	remove_duplicates(Os,Os2),
	costQuestion(CQ),
	costAnswersObject(Pn,Care,Os2,ACs,Level),
	expectedCost(ACs,MinC),
	TC is CQ + MinC.

typeQuestionBranch(Pn,Care,TC,Level) :-
	nth1(1,Care,nocare),
	TC = 10000.
typeQuestionBranch(Pn,Care,TC,Level) :-
	length(Pn,Le),
	Le==1,
	TC = 10000.
typeQuestionBranch(Pn,Care,TC,Level) :-
	nth1(1,Care,care),
	findall(Type,(member((_:(Type,_)),Pn)),Types),
	remove_duplicates(Types,Types2),
	length(Types2,Le),
	Le==1,
	TC = 10000.
typeQuestionBranch(Pn,Care,TC,Level) :-
	nth1(1,Care,care),
	findall(Type,(member((_:(Type,_)),Pn)),Types),
	nth1(1,Care,TCare),
	costQuestion(CQ),
	costAnswersType(Pn,Care,Types,ACs,Level),
	expectedCost(ACs,MinC),
	TC is CQ + MinC.

toolQuestionBranch(Pn,Care,TC,Level) :-
	nth1(3,Care,nocare),
	TC = 10000.
toolQuestionBranch(Pn,Care,TC,Level) :-
	length(Pn,Le),
	Le==1,
	TC = 10000.
toolQuestionBranch(Pn,Care,TC,Level) :-
	nth1(3,Care,care),
	findall(T,(member((_:(_,[_,_,_,_,T])),Pn)),Types),
	remove_duplicates(Types,Types2),
	length(Types2,Le),
	Le==1,
	TC = 10000.
toolQuestionBranch(Pn,Care,TC,Level) :-
	nth1(3,Care,care),
	findall(T,(member((_:(_,[_,_,_,_,T])),Pn)),Types),
	costQuestion(CQ),
	costAnswersTool(Pn,Care,Types,ACs,Level),
	expectedCost(ACs,MinC),
	TC is CQ + MinC.


/*
Cost calculations
*/

costAction(pickup,5).
costAction(place,500).
costAction(info,300).%NP dep on situation
costAction(nothing,0).
costAction(posfeedback,1).
costQuestion(400).
costAnswer(1).
reverseAction(pickup,place).
reverseAction(place,pickup).
reverseAction(info,nothing).

%cost weighted with probability of event
expectedCost([],0).
expectedCost([(Prob:Cost)|R],MinC) :-
	Cost2 is Prob*Cost,
	expectedCost(R,MinC2),
	MinC is MinC2 + Cost2.

%cost weighted with probability of answer (equivalent to chance of being in tuple of WA distribution)
costAnswersObject(_,_,[],[],_).
costAnswersObject(Pn,Care,[A|AR],[Sum:CostA|CostR],Level) :-
	findall(X,(member(X,Pn),X = (_:(_,[A,_,_,_,_]))),List),
	probabilities_sum(List,Sum),
	relative_probs(List,Sum,List2),
	getMinCost(List2,Care,MinC,Level),
	costAnswer(CgiveA),
	CostA is MinC + CgiveA,
	costAnswersObject(Pn,Care,AR,CostR,Level).
costAnswersType(_,_,[],[],Level).
costAnswersType(Pn,Care,[A|AR],[Sum:CostA|CostR],Level) :-
	findall(X,(member(X,Pn),X = (_:(A,_))),List),
	probabilities_sum(List,Sum),
	relative_probs(List,Sum,List2),
	getMinCost(List2,Care,MinC,Level),
	costAnswer(CgiveA),
	CostA is MinC + CgiveA,
	costAnswersType(Pn,Care,AR,CostR,Level).
costAnswersTool(_,_,[],[],Level).
costAnswersTool(Pn,Care,[A|AR],[Sum:CostA|CostR],Level) :-
findall(X,(member(X,Pn),X = (_:(_,[_,_,_,_,A]))),List),
	probabilities_sum(List,Sum),
	relative_probs(List,Sum,List2),
	getMinCost(List2,Care,MinC,Level),
	costAnswer(CgiveA),
	CostA is MinC + CgiveA,
	costAnswersTool(Pn,Care,AR,CostR,Level).

%cost weighted with probability of action WA
weighActions(_,[],[]).
weighActions(Prob,[H|R],[H2|R2]) :-
	H = (Cost:Re),
	WeighedCost is Cost*Prob,	
	H2 = (WeighedCost:Re),
	weighActions(Prob,R,R2).

%get the option/branch with the minimum expected cost over list of branches
getMinCost([(Min:Amin)],Care,Min,Level).
getMinCost(Pn2,Care,0,Level) :-
	Level2 is Level -1,
	possibleActions(Pn2,Care,[],Level2).
getMinCost(Pn2,Care,Min,Level) :-
	Level2 is Level -1,
	possibleActions(Pn2,Care,Actions,Level2),
	findall((Prob), member((Prob:Action),Actions),L),
	min_list(L,Min).

%calculation of all (still) possible care combinations, used to generate different begin states
possCares(_,[],[(1:[])]).
possCares(Hu,[H|R],Res) :-
	unknowncare(Hu,H),
	possCares(Hu,R,ResLow),
	priorCare(H,Care),
	extendCareList(ResLow,[care],Care,Res2),
	NoCare is 1-Care,
	extendCareList(ResLow,[nocare],NoCare,Res3),
	append(Res2,Res3,Res).
possCares(Hu,[H|R],Res) :-
	care(Hu,H),
	possCares(Hu,R,R2),
	extendCareList(R2,[care],1,Res).
possCares(Hu,[H|R],Res) :-
	nocare(Hu,H),
	possCares(Hu,R,R2),
	extendCareList(R2,[nocare],1,Res).
extendCareList([],_,_,[]).
extendCareList([H|R],Care,Prob,[H2|R2]) :-
	H = (Prob2:L),
	Prob3 is Prob2*Prob,
	append(Care,L,L2),
	H2 = (Prob3:L2),
	extendCareList(R,Care,Prob,R2).

getCareFormula([],[],[]).
getCareFormula([H|R],[H2|R2],[H3|R3]) :-
	H2 = care,
	H3 = H,
	getCareFormula(R,R2,R3).
getCareFormula([H|R],[H2|R2],[H3|R3]) :-
	H2 = nocare,
	getCareFormula(R,R2,R3).

%weighting of the calculated best actions with all possible begin states at the end of the construction procedure
addCosts(Actions,Actions2) :-
	findall((Tu),member((_:Tu),Actions),LR),
	remove_duplicates(LR,LRP),
	addCosts2(Actions,LRP,Actions2).
addCosts2(_,[],[]).
addCosts2(Actions,[H|R],[H2|R2]) :-
	findall(Prob,(member((Prob:H),Actions)),Probs),
	sum_list(Probs,Sum),
	H2 = (Sum:H),
	addCosts2(Actions,R,R2).

%this simple method picks the most probable action as the one to perform
get_best_waction(BestAction,N) :-
	set_algorithm(argmax),
	H = human(1),%NP
	timestep(Time),
	get_perfname(Time,Name),
	eval_query_particle2(((Name,AT,[F,X,Y,Z,Params])),(current(wanted_action(H))~=(AT,F,Params),current(has_position(F))~=(X,Y,Z)),N,R2e),
	get_probs(R2e,Probs),
	max_list(Probs,Best),
	member((Best:BestAction),R2e),
	BestAction = (Na,At,Pr),
	nl,write('Selected best action is: '),writeln(BestAction),
	add_new_evidence([perf(Na,At,Pr)]).

%extra utilities for input processing
get_best_waction(Hu,BestAction,N) :-
	get_best_waction(BestAction,N).
processDistrActionFeed(Pn,Care,H,Pn2) :-
	deleteElement(H,Pn,Pn3),
	probabilities_sum(Pn3,Sum),
	relative_probs(Pn3,Sum,Pn2).

%%%%%%%%%%%%%%%%%%%%%%%%
%%Deprecated, now in particles themselves

waCare(_,[],[]).
waCare(Human,[H1|R1],[0|R2]) :-
	nocare(Human,H1),
	waCare(Human,R1,R2).
waCare(Human,[H1|R1],[1|R2]) :-
	care(Human,H1),
	waCare(Human,R1,R2). 
waCare(Human,[type|R1],[1|R2]) :-
	unknowncare(Human,H1),
	waCare(Human,R1,R2).
waCare(Human,[object|R1],[0.8|R2]) :-
	unknowncare(Human,H1),
	waCare(Human,R1,R2).
waCare(Human,[tool|R1],[0.5|R2]) :-
	unknowncare(Human,H1),
	waCare(Human,R1,R2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%
