%%% -*- Mode: Prolog; -*-
:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module('knowledge_base.pl').
:- use_module('knowledge_processing.pl').
:- use_module('init.pl').
:- use_module('color_processing.pl').
:- use_module('action_decision.pl').
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- set_magic(backward). 

lifted(false).
raoblackwellisation(false).

s_test :-
	N is 100,
	initdcpf(N),
	gripper_location(left_gripper,[0.34999999999999998,0.75,1.05],N),
	gripper_location(right_gripper,[0.29999999999999999,-0.75,1.05],N),

	tableObservation([0.65685790655403253,0.99562966810066333,0.20000000000000001],[0.667174459,0.00335927983,0.383494049],N),
	nobjsObservationInit(3,N),
	colorObservationInit(1,[165,109,49],N),
	locationObservationInit(1,[0.0587398708,0.184396714,0.113916188],[0.506681323,-0.092198357,0.550515056],N),
	colorObservationInit(2,[159,100,42],N),
	locationObservationInit(2,[0.051786989,0.0564360768,0.133395076],[0.509086907,0.0652225092,0.560042858],N),
	colorObservationInit(3,[169,113,58],N),
	locationObservationInit(3,[0.0183226466,0.0599130988,0.113717496],[0.986414909,-0.00238251034,0.550136209],N),
	typeObservationInit(1,[1671.02026:18744,888.998657:18797,394.073578:18802,334.811432:18783,300.64505:18748,209.941071:18751,158.177353:18807,155.289688:18659,144.582993:18658,139.131119:18646,130.260864:18652,128.140808:18951,122.353951:18704,117.378662:18794,81.9690552:18645,79.0995865:18760],N),

	typeObservationInit(2,[1151.97546:18802,367.045074:18783,261.034973:18744,233.462433:18797,172.030945:18646,167.819321:18751,163.24115:18748,118.238739:18807,117.543571:18704,113.892715:18659,113.124802:18794,108.859703:18658,108.697365:18951,99.0563049:18760,73.1665573:18652,69.7457886:18645],N),

typeObservationInit(3,[760.38739:18802,342.146149:18646,279.619324:18704,279.450043:18951,237.989334:18659,204.906769:18658,190.323547:18794,189.898941:18760,165.755875:18751,165.334503:18748,145.574585:18783,137.10524:18652,126.356773:18807,97.3773041:18645,74.3472595:18744,73.5606003:18797],N),


needBeginObservation(human(1),N),
actionDemandObservation(human(1),N),
commandObservation(human(1),[],N),
get_best_waction((Name,Cat,Params),N),

start_performance(Name,N),
end_performance(Name,N),

	pos_wanted_effect(Name,N),
	positive_feedback(Name,N).


%scenario1objects :-
s1 :-
	N is 1000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(1,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),
		
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [],

	printp(1),

	commandObservation(human(1),Command,N),

	
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),
	pos_wanted_effect(Name,N),
	positive_feedback(Name,N).


%scenario2objects :-
s2 :-
	N is 1000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(2,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),

	colorObservationInit(2,[73,21,12],N),
	locationObservationInit(2,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

		
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [],

	commandObservation(human(1),Command,N),
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),
	pos_wanted_effect(Name,N).

%scenario3objects :-
s3 :-
	N is 1000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),

	colorObservationInit(2,[73,21,12],N),
	locationObservationInit(2,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

	colorObservationInit(3,[73,21,12],N),
	locationObservationInit(3,[0.0191877484,0.0611744151,0.113761216],[0.386119509,-0.00237884745,0.549472451],N),
	typeObservationInit(3,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

		
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [],

	commandObservation(human(1),Command,N),
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),
	pos_wanted_effect(Name,N).

s8 :-
	N is 1000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(8,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),

	colorObservationInit(2,[73,21,12],N),
	locationObservationInit(2,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

	colorObservationInit(3,[73,21,12],N),
	locationObservationInit(3,[0.0191877484,0.0611744151,0.113761216],[0.386119509,-0.00237884745,0.549472451],N),
	typeObservationInit(3,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

	colorObservationInit(4,[73,21,12],N),
	locationObservationInit(4,[0.0191877484,0.0611744151,0.113761216],[1000,1000,1000],N),
	typeObservationInit(4,[1112.17334:18645],N),

	colorObservationInit(5,[73,21,12],N),
	locationObservationInit(5,[0.0191877484,0.0611744151,0.113761216],[1000,1000,1000],N),
	typeObservationInit(5,[1112.17334:18645],N),

	colorObservationInit(6,[73,21,12],N),
	locationObservationInit(6,[0.0191877484,0.0611744151,0.113761216],[1000,1000,1000],N),
	typeObservationInit(6,[1112.17334:18645],N),

	colorObservationInit(7,[73,21,12],N),
	locationObservationInit(7,[0.0191877484,0.0611744151,0.113761216],[1000,1000,1000],N),
	typeObservationInit(7,[1112.17334:18645],N),

	colorObservationInit(8,[73,21,12],N),
	locationObservationInit(8,[0.0191877484,0.0611744151,0.113761216],[1000,1000,1000],N),
	typeObservationInit(8,[1112.17334:18645],N),
		
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [],

	commandObservation(human(1),Command,N),
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),
	pos_wanted_effect(Name,N).

s_uniform :-
	N is 100,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),

	colorObservationInit(2,[73,21,12],N),
	locationObservationInit(2,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [],
	commandObservation(human(1),Command,N).

s_test1 :-
	N is 100,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(2,N),

	colorObservationInit(1,[252,3,1],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),

	colorObservationInit(2,[73,21,12],N),
	locationObservationInit(2,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	
Command = [has_colour(object(ID),red),current(has_category(cup,object(ID)))~=true,current(wanted_action(human(1)))~=(AT,object(ID),T)],
	commandObservation(human(1),Command,N).

s_staticcomm :-
	N is 100,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(2,N),

	colorObservationInit(1,[252,3,1],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),

	colorObservationInit(2,[73,21,12],N),
	locationObservationInit(2,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [(has_colour(object(ID),red))],
	commandObservation(human(1),Command,N).

s_wantfluid :-
	N is 200,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(2,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),

	colorObservationInit(2,[73,21,12],N),
	locationObservationInit(2,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

		
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [current(wants_drinkablefluid(human(1)))~=true],

	commandObservation(human(1),Command,N),
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),
	pos_wanted_effect(Name,N).

s_uniform2 :-
	N is 2,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.5,1],N),
	
	
	tableObservation([0.69018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(1,N),

	colorObservationInit(1,[255,0,0],N),
	locationObservationInit(1,[0.0308000147,0.0650024116,0.1013891768],[0.49119633,0.00733181741,0.552862129],N),
	typeObservationInit(1,	[42:18783,6.5:18802,0:18760,0:18744,0:18797,25.5:18951,0:18807,0:18645,0:18704,0:18748,25:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),
	
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N).
	

s_voordelen :-
	N is 2000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.7,1],N),
	
	tableObservation([0.69018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),


	%classificatie verkregen na perceptie-iteratie in simulatie, zie voordelen.launch, roteer hendel mugs weg van visueel veld en verplaats can iets verder van robot; scores samengevat per categorie

	colorObservationInit(1,[255,0,0],N),
	locationObservationInit(1,[0.0308000147,0.0650024116,0.1013891768],[0.42119633,0.00733181741,0.552862129],N),
	typeObservationInit(1,	[42:18783,6.5:18802,0:18760,0:18744,0:18797,25.5:18951,0:18807,0:18645,0:18704,0:18748,25:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	colorObservationInit(2,[0,128,0],N),
	locationObservationInit(2,[0.0436866593,0.0742595494,0.100945175],[0.497390981,-0.000369073823,0.543933868],N),
	typeObservationInit(2,[29:18783,24:18802,22.5:18704,0:18748,24.5:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	colorObservationInit(3,[0,128,0],N),
	locationObservationInit(3,[0.00548326969,0.0392764658,0.100079298],[0.993370056,-0.000708418898,0.54429239],N),
	typeObservationInit(3,[26:18783,21.5:18802,25:18704,0:18748,27.5:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N),

	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	
	%"I want a drink"
	Command = [current(wants_drinkablefluid(human(1)))~=true],
	commandObservation(human(1),Command,N),
	
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),

	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N),

	%printp(1),
	pos_wanted_effect(Name,N),
	positive_feedback(Name,N),

	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N),

	%new demand from user
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N).

s_voordelen2 :-
	N is 2000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.7,1],N),
	
	tableObservation([0.69018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),


	%classificatie verkregen na perceptie-iteratie in simulatie, zie voordelen.launch, roteer hendel mugs weg van visueel veld en verplaats can iets verder van robot; scores samengevat per categorie

	colorObservationInit(1,[255,0,0],N),
	locationObservationInit(1,[0.0308000147,0.0650024116,0.1013891768],[0.42119633,0.00733181741,0.552862129],N),
	typeObservationInit(1,	[42:18783,6.5:18802,0:18760,0:18744,0:18797,25.5:18951,0:18807,0:18645,0:18704,0:18748,25:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	colorObservationInit(2,[0,128,0],N),
	locationObservationInit(2,[0.0436866593,0.0742595494,0.100945175],[0.497390981,-0.000369073823,0.543933868],N),
	typeObservationInit(2,[29:18783,24:18802,22.5:18704,0:18748,24.5:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	colorObservationInit(3,[0,128,0],N),
	locationObservationInit(3,[0.00548326969,0.0392764658,0.100079298],[0.993370056,-0.000708418898,0.54429239],N),
	typeObservationInit(3,[26:18783,21.5:18802,25:18704,0:18748,27.5:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N),

	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	
	%"I want a drink"
	Command = [current(wants_drinkablefluid(human(1)))~=true],
	commandObservation(human(1),Command,N),
	
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),

	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N),

	%printp(1),
	pos_wanted_effect(Name,N),

	negative_feedback(N,CA),
	writeln(comp_act:CA),
	CA = (Name2,Cat2,Params2),
	%performance_current_query(N),
	start_performance(Name2,N),
	success_performance(Name2,N),
	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N).

	%new demand from user
	%needBeginObservation(human(1),N),
	%actionDemandObservation(human(1),N).

s_voordelen3 :-
	N is 2000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.7,1],N),
	
	tableObservation([0.69018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),


	%classificatie verkregen na perceptie-iteratie in simulatie, zie voordelen.launch, roteer hendel mugs weg van visueel veld en verplaats can iets verder van robot; scores samengevat per categorie

	colorObservationInit(1,[255,0,0],N),
	locationObservationInit(1,[0.0308000147,0.0650024116,0.1013891768],[0.42119633,0.00733181741,0.552862129],N),
	typeObservationInit(1,	[42:18783,6.5:18802,0:18760,0:18744,0:18797,25.5:18951,0:18807,0:18645,0:18704,0:18748,25:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	colorObservationInit(2,[0,128,0],N),
	locationObservationInit(2,[0.0436866593,0.0742595494,0.100945175],[0.497390981,-0.000369073823,0.543933868],N),
	typeObservationInit(2,[29:18783,24:18802,22.5:18704,0:18748,24.5:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	colorObservationInit(3,[0,128,0],N),
	locationObservationInit(3,[0.00548326969,0.0392764658,0.100079298],[0.993370056,-0.000708418898,0.54429239],N),
	typeObservationInit(3,[26:18783,21.5:18802,25:18704,0:18748,27.5:18659,0:18652,0:18646,0:18658,0:18794,0:18751],N),

	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N),

	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	
	%"Mug"
	Command = [current(has_category(mug,F))~=true],
	commandObservation(human(1),Command,N),
	
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),

	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N),

	%printp(1),
	pos_wanted_effect(Name,N),

	negative_feedback(N,CA),
	writeln(comp_act:CA),
	CA = (Name2,Cat2,Params2),
	%performance_current_query(N),
	start_performance(Name2,N),
	success_performance(Name2,N),
	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N)
	.

s_vaagheid :-
	N is 200,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.7,1],N),
	
	tableObservation([0.69018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),


	%classificatie verkregen na perceptie-iteratie in simulatie, zie voordelen.launch, roteer hendel mugs weg van visueel veld en verplaats can iets verder van robot; scores samengevat per categorie

	colorObservationInit(1,[255,0,0],N),
	locationObservationInit(1,[0.0308000147,0.0650024116,0.1013891768],[0.42119633,0.00733181741,0.552862129],N),
	typeObservationInit(1,	[100:18783],N),

	colorObservationInit(2,[0,128,0],N),
	%locationObservationInit(2,[0.0436866593,0.0742595494,0.100945175],[0.497390981,-0.000369073823,0.543933868],N),
	locationObservationInit(2,[0.0436866593,0.0342595494,0.080945175],[0.607390981,-0.000369073823,0.543933868],N),
	typeObservationInit(2,[100:18659],N),

	colorObservationInit(3,[0,128,0],N),
	locationObservationInit(3,[0.00548326969,0.0392764658,0.100079298],[0.993370056,-0.000708418898,0.54429239],N),
	%locationObservationInit(3,[0.00548326969,0.0392764658,0.060079298],[0.993370056,-0.000708418898,0.54429239],N),
	typeObservationInit(3,[100:18659],N),

	nl,
	obj_current_query(object(1),N),
	nl,	
	obj_current_query(object(2),N),
	nl,	
	obj_current_query(object(3),N),

	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	
	%Command = [],
	%Command = [current(wants_object(fluidholders_ca,human(N2)))~=true],
	%Command = [current(wants_water(human(N2))) ~=true],	
	%Command = [current(has_focus(human(N2))) ~= O, current(has_category(can,O)) ~=true],
	Command = [current(has_craving(human(N2),thirst)) ~= true],
	%Command = [current(has_need(human(N2))) ~= info],
	%Command = [current(wants_to_move(human(N2),Obj)) ~=true,current(bigger(Obj,_))~=true],
	%Command = [current(wants_pickup(human(N2),O)) ~=true,current(has_category(can,O)) ~=true],
	%Command = [current(wants_to_move(human(N2),O1)) ~=true, current(has_category(mug,O1)) ~=true,current(has_category(mug,O2)) ~=true,current(frontof(O1,O2))~=true],
	%Command = [current(wants_info_on(human(N2),object(O)))~=true,has_colour(object(O),green),current(has_category(mug,object(O)))~=true],
	%Command = [current(wants_pickup(human(N2),O)) ~=true,has_colour(O,red),current(has_category(can,O))~=true,current(wants_tool_used(human(N2))) ~=right_gripper],	
		
	commandObservation(human(1),Command,N).

s_final :-
	N is 300,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),

	tableObservation([0.43293549648544905,1.0000509571382328,0.20000000000000001],[0.565403521,-0.0135424221,0.383583158],300),
nobjsObservationInit(3,300),

colorObservationInit(1,[132,132,132],300),
locationObservationInit(1,[0.0756790638,0.0748852044,0.182548463],[0.56488359,0.0519318879,0.584469438],300),

colorObservationInit(2,[132,132,132],300),
locationObservationInit(2,[0.0509822369,0.18261604,0.133446485],[0.65822798,-0.0913080201,0.559425592],300),

colorObservationInit(3,[178,123,67],300),
locationObservationInit(3,[0.0594999492,0.226586014,0.0688352883],[0.415134132,-0.113293007,0.527897596],300),

typeObservationInit(1,[174.747299:18751,170.985641:18704,102.089622:18794,80.175621:18646,73.041954:18760,72.8117142:18802,65.2802048:18783,55.5239639:18797,54.0129509:18744,53.0407372:18951,52.8059196:18748,42.9386559:18659,42.8148651:18807,42.4377785:18658,41.3977737:18652,36.9649353:18645],300),

typeObservationInit(2,[1022.01971:18802,355.067017:18783,272.175934:18744,240.307709:18797,224.945038:18751,177.130951:18646,164.560486:18748,148.400513:18794,119.876083:18807,118.894859:18704,115.288681:18659,112.577293:18951,109.30925:18658,106.788849:18760,71.544487:18645,67.2270203:18652],300),

typeObservationInit(3,[197.776138:18783,185.021286:18802,138.806961:18760,119.982887:18744,117.474373:18797,117.169273:18807,116.629044:18951,109.259674:18645,95.6758194:18704,93.6597824:18659,93.5960922:18646,93.1897049:18748,92.4684372:18652,92.0649948:18658,90.8451385:18794,84.1170731:18751],300),


objectsObservationIterationParticles(4,[[0.575578094,0.0503352731,0.584985852,0.0740410686,0.0752659142,0.182742149,12,21,13,[173.926239:18751,171.725418:18704,82.6954727:18646,74.7577286:18802,74.0794373:18760,67.3953781:18783,57.494751:18797,55.7714424:18744,54.5838661:18951,54.4291077:18748,43.7800064:18659,43.6748352:18807,43.2476044:18658,42.1255569:18652,39.2555008:18794,37.3966179:18645]],[0.669757724,-0.0919826105,0.560163438,0.0515041947,0.183965221,0.133380502,8,8,0,[992.204773:18802,365.020599:18783,279.867859:18744,243.87056:18797,235.302155:18751,189.720642:18646,166.900986:18748,147.220154:18794,125.172646:18807,123.35202:18704,118.646355:18659,115.446602:18951,111.983772:18658,104.710625:18760,72.7540894:18645,66.3929291:18652]],[0.426312327,-0.114501961,0.528092861,0.0594780445,0.229003921,0.0693264604,156,156,156,[199.843903:18783,186.571701:18802,141.247589:18760,119.763168:18744,119.400024:18797,116.645439:18951,115.786812:18807,108.350235:18645,94.7814713:18704,93.545639:18652,93.1743393:18646,93.1597824:18659,93.111496:18748,91.6752548:18658,89.1417694:18794,83.9750671:18751]],[0.390084684,-0.00457398221,0.598680258,0.0266584158,0.0609907433,0.0178399682,24,9,9,[713.734924:18744,339.803833:18802,250.637436:18783,233.966187:18797,225.887222:18646,208.872772:18704,205.956589:18794,197.275558:18760,177.459:18748,156.185425:18751,84.3416214:18658,82.2662811:18659,77.9194641:18652,71.4845276:18807,42.119812:18951,42.0711823:18645]]],300),

needBeginObservation(human(1),300),
actionDemandObservation(human(1),300),
commandObservation(human(1),[],300),
get_best_waction(BA,300),
needBeginObservation(human(1),300),
actionDemandObservation(human(1),300),
commandObservation(human(1),[],300),
get_best_waction(BA2,300).

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%query_effect(Name,N),
	
	
	%neg_wanted_effect(Name,N),
	%robot_current_query(N),
	%query_effect(Name,N),
	%negative_feedback(N,CA),
	%Params = [O,_,_,_,Gripper],
	%gripper_location(left_gripper,[100,100,100],N).%random.	

scenario2 :-
	N is 200,
	initdcpf(N),
	gripper_location(left_gripper,[20,20,20],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0614038706,0.0697266757,0.114438295],[0.591368258,-0.00331052206,0.569395304],N),
	
	typeObservationInit(1,[1112.17334:can,381.052765:bottle,371.650635:bottle,357.987183:can,329.872711:can,324.367859:bottle,297.535095:can,216.147034:bottle,196.884735:can,162.84436:can,125.140213:cup,113.481651:glass,101.011917:bottle,92.9746933:glass,89.1350327:jar,44.4899979:bowl,34.4265366:bowl],N),
	

	colorObservationInit(2,[87,38,26],N),
	locationObservationInit(2,[0.0599404573,0.0635898823,0.114453733],[0.89042896,-0.117949411,0.568698406],N),
	typeObservationInit(2,[1142.30457:can,418.744843:bottle,385.833282:bottle,375.212982:can,354.599731:can,329.078583:bottle,305.137573:can,226.849152:bottle,198.223373:can,171.178619:can,132.107391:cup,117.464798:glass,116.075165:bottle,96.1259613:glass,94.0123596:jar,46.062645:bowl,36.25811:bowl],N),

	colorObservationInit(3,[92,46,36],N),
	locationObservationInit(3,[0.059751749,0.068519786,0.114168465],[0.890725136,0.195666254,0.568740487],N),
	typeObservationInit(3,[1052.79773:can,387.281616:bottle,383.460663:bottle,346.348938:can,336.140594:can,319.713104:bottle,288.598907:can,208.809189:bottle,198.760818:can,164.122574:can,125.466606:cup,115.384842:glass,94.6608124:glass,92.8416824:jar,85.7993393:bottle,45.4006691:bowl,35.4842796:bowl],N),
	
	%Command = [current(has_craving(human(1),thirst)) ~= true],
	%Command = [current(has_need(human(1))) ~= info],
		
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [current(has_category(can,F))~=true,current(wanted_action(human(1)))~=(pickup,F,_)],

	commandObservation(human(1),Command,N),
	get_best_waction(human(1),(Name,Cat,Params),N),
	
	%get_best_waction(human(1),N),

	most_informative_question(N),
	give_answer(object(3),object,N),
	current_wa_query(human(1),N),	

	

	start_performance(Name,N),
	performance_current_query(N),
	
	success_performance(Name,N),%C++
	Params = [O,_,_,_,Gripper],
	gripper_location(left_gripper,[100,100,100],N),%random
	obj_position_current_query(O,N),
	
	%robot_current_query(N),
	%positive_feedback(Name,N),
	negative_feedback(N,CA),
	writeln(comp_act:CA),
	CA = (Name2,Cat2,Params2),
	%performance_current_query(N),
	start_performance(Name2,N),
	success_performance(Name2,N),%C++
	/*
	objectsObservationIterationParticles(1,

[

%[0.890841484,0.195653468,0.570687592,0.0586682558,0.0684881657,0.114521861,51,0,0,[1058.06873:can,381.290344:bottle,380.837158:bottle,349.675812:can,330.817291:can,317.283752:bottle,285.471802:can,207.949463:bottle,195.716217:can,163.295074:can,124.187294:cup,114.278328:glass,94.2958069:glass,91.8283005:jar,85.1397858:bottle,45.2459183:bowl,35.4093857:bowl]]
%,
[0.890841484,0.195653468,0.570687592,0.0586682558,0.0684881657,0.114521861,51,0,0,[1058.06873:can,381.290344:bottle,380.837158:bottle,349.675812:can,330.817291:can,317.283752:bottle,285.471802:can,207.949463:bottle,195.716217:can,163.295074:can,124.187294:cup,114.278328:glass,94.2958069:glass,91.8283005:jar,85.1397858:bottle,45.2459183:bowl,35.4093857:bowl]]
	
],N),
	writeln(endddd),
	*/	
	current_wa_query(human(1),N),
	obj_queries([1,2,3],N).


	/*
	objectsObservationIterationParticles(3,

[
%2
[0.891220212,-0.118366361,0.570792437,0.0612149835,0.236732721,0.114446163,51,0,0,[1095.2959:can,407.322815:bottle,369.061676:bottle,368.468994:can,346.120209:can,321.058899:bottle,293.723114:can,216.318359:bottle,193.658997:can,166.382278:can,128.084061:cup,115.625893:glass,109.443977:bottle,94.7402039:glass,91.9764481:jar,45.500885:bowl,35.8342552:bowl]]
,
[0.890841484,0.195653468,0.570687592,0.0586682558,0.0684881657,0.114521861,51,0,0,[1058.06873:can,381.290344:bottle,380.837158:bottle,349.675812:can,330.817291:can,317.283752:bottle,285.471802:can,207.949463:bottle,195.716217:can,163.295074:can,124.187294:cup,114.278328:glass,94.2958069:glass,91.8283005:jar,85.1397858:bottle,45.2459183:bowl,35.4093857:bowl]]
,
[0.59112525,-0.00307270978,0.570950031,0.060151577,0.0686057359,0.114087522,51,0,0,[1166.02283:can,374.99411:bottle,369.463074:bottle,344.579559:can,323.909851:can,310.989655:bottle,277.815796:can,203.861664:bottle,189.332016:can,155.216827:can,119.962456:cup,113.895485:glass,101.16758:bottle,93.5784912:glass,85.9296722:jar,43.3196983:bowl,33.8908997:bowl]]	
	
],
	N).
	*/	

s_zelfde :-
	N is 200,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.5,1],N),

	obj_position_queries([1,2,3,4,5,6,7,8],N),
	tableObservation([0.43656513684221837,1.002719400032511,0.20000000000000001],[0.564232528,-0.0127394339,0.383607566],N),

	nobjsObservationInit(1,N),

	colorObservationInit(1,[177,157,137],N),
	locationObservationInit(1,[0.0593472719,0.0670648813,0.113910168],[0.506999254,-0.00302657206,0.550401211],N),
	typeObservationInit(1,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]

,N),

obj_position_queries([1,2,3,4,5,6,7,8],N),

	objectsObservationIterationParticles(1,[[0.519673705,-0.00708872639,0.550745904,0.0598632991,0.0673424304,0.11365518,28,14,14,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]]],N),
	obj_position_queries([1,2,3,4,5,6,7,8],N),	
	writeln(end).

s_meer :-
	N is 200,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),

	obj_position_queries([1,2,3,4,5,6,7,8],N),
	tableObservation([0.43656513684221837,1.002719400032511,0.20000000000000001],[0.564232528,-0.0127394339,0.383607566],N),

	nobjsObservationInit(1,N),

	colorObservationInit(1,[177,157,137],N),
	locationObservationInit(1,[0.0593472719,0.0670648813,0.113910168],[0.506999254,-0.00302657206,0.550401211],N),
	typeObservationInit(1,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]

,N),
	obj_position_queries([1,2,3,4,5,6,7,8],N),

	objectsObservationIterationParticles(2,[
[0.519673705,-0.00708872639,0.550745904,0.0598632991,0.0673424304,0.11365518,28,14,14,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]],
[0.659673705,-0.00708872639,0.550745904,0.0598632991,0.0673424304,0.11365518,28,14,14,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]]

],N),
	writeln(test),
	obj_position_queries([1,2,3,4,5,6,7,8],N),	
	writeln(end).

s_minder_meer :-
	N is 2000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),


	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0614038706,0.0697266757,0.114438295],[0.591368258,-0.00331052206,0.569395304],N),
	
	typeObservationInit(1,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]

,N),

	colorObservationInit(2,[87,38,26],N),
	locationObservationInit(2,[0.0599404573,0.0635898823,0.114453733],[0.89042896,-0.117949411,0.568698406],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]

,N),
	colorObservationInit(3,[92,46,36],N),
	locationObservationInit(3,[0.059751749,0.068519786,0.114168465],[0.890725136,0.195666254,0.568740487],N),
	typeObservationInit(3,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]

,N),

	obj_position_queries([1,2,3,4,5],N),


	objectsObservationIterationParticles(2,[
[0.591368258,-0.00331052206,0.569395304,0.0614038706,0.0697266757,0.114438295,28,14,14,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]],
[0.89042896,-0.117949411,0.568698406,0.0599404573,0.0635898823,0.114453733,28,14,14,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]]

],N),
	obj_position_queries([1,2,3,4,5],N),

	objectsObservationIterationParticles(4,[
[0.591368258,-0.00331052206,0.569395304,0.0614038706,0.0697266757,0.114438295,28,14,14,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]],
[0.89042896,-0.117949411,0.568698406,0.0599404573,0.0635898823,0.114453733,28,14,14,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797]],
[0.890725136,0.195666254,0.568740487,0.059751749,0.068519786,0.114168465,28,14,14,[20.713257:18783,0:18802,139.080078:18760,8.721001:18744,0:18797]],
[0.890725136,0.195666254,0.568740487,0.059751749,0.068519786,0.114168465,28,14,14,[20.713257:18783,0:18802,139.080078:18760,8.721001:18744,0:18797]]


],N),
	obj_position_queries([1,2,3,4,5],N),

	step_particle([],[observation(nobjsit)~=5],N),
	
	obj_position_queries([1,2,3,4,5],N),

	writeln(end).

scenario3 :-
	N is 200,
	initdcpf(N),
	gripper_location(left_gripper,[20,20,20],N),
	gripper_location(right_gripper,[0.20000000000000001,-0.69999999999999996,1],N),
	
	tableObservation([0.59018577638331537,1.3934393250213899,0.20000000000000001],[0.702449799,0.0959949195,0.4035106],N),
	nobjsObservationInit(3,N),

	colorObservationInit(1,[73,21,12],N),
	locationObservationInit(1,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(1,[1112.17334:18645],N),

	colorObservationInit(2,[73,21,12],N),
	locationObservationInit(2,[0.0191877484,0.0611744151,0.113761216],[0.486119509,-0.00237884745,0.549472451],N),
	typeObservationInit(2,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

	colorObservationInit(3,[73,21,12],N),
	locationObservationInit(3,[0.0191877484,0.0611744151,0.113761216],[0.386119509,-0.00237884745,0.549472451],N),
	typeObservationInit(3,[193.713257:18783,182.929535:18802,139.080078:18760,118.721001:18744,117.522446:18797,114.794281:18951,113.656448:18807,108.063454:18645,95.3420792:18704,93.5516281:18748,93.3007126:18659,92.9673615:18652,92.561554:18646,91.007843:18658,89.4134674:18794,84.4100876:18751],N),

		
	needBeginObservation(human(1),N),
	actionDemandObservation(human(1),N),
	Command = [],

	commandObservation(human(1),Command,N),
	get_best_waction(human(1),(Name,Cat,Params),N),
	start_performance(Name,N),
	end_performance(Name,N),
	
	query_effect(Name,N),
	
	pos_wanted_effect(Name,N),%C++
	%neg_wanted_effect(Name,N),%C++
	robot_current_query(N),
	query_effect(Name,N),
	%negative_feedback(N,CA),
	Params = [O,_,_,_,Gripper],
	gripper_location(left_gripper,[100,100,100],N).%random.	

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

s_test :-
	N is 2000,
	initdcpf(N),
	gripper_location(left_gripper,[0.20000000000000001,0.5,1],N),
	gripper_location(right_gripper,[0.29999999999999999,-0.75,1.05],2000),
	tableObservation([0.6503485180276567,0.99559834176184281,0.20000000000000001],[0.670519233,0.00333563751,0.38350597],2000),

	nobjsObservationInit(3,2000),
	locationObservationInit(1,[0.0590888262,0.184062287,0.113773733],[0.506228745,-0.0920311436,0.550198793],2000),
	locationObservationInit(2,[0.0543677211,0.0574410222,0.133281916],[0.509840131,0.0650972351,0.559805334],2000),
	locationObservationInit(3,[0.019020021,0.0607084334,0.113789916],[0.986155272,-0.00236570463,0.549385309],2000),
	
	actionDemandObservation(human(1),2000),
	commandObservation(human(1),[],2000).


	

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
