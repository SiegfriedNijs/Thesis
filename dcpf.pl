%%% -*- Mode: Prolog; -*-

:- module(dcpf,[assert_list/2,printkeyp/1,printkey/1,step_particle_aux/5,step_particle3/3,eval_average_particle/5,eval_query_particle3/5,logIndepOptimalProposals/3,indepOptimalProposals/3,optimalproposal/7,likelihood_weighting/3,normalize/2,printp/1,printp/0,set_magic/1,step_particle/3,step_particle/4,step_particle/5,step_particle_ps/2,step_particle_ps/3,step_particle_ps/4,eval_query_particle/3,eval_query_particle2/4,init_particle/1,init_particle/2,init_particle/3]).

:- use_module('random/sampling.pl').
:- use_module(library(lists)).
:- use_module(distributionalclause).
:- use_module(library(charsio)).

:- bb_put(initdcpf,false).

printp(Pos) :-
	dcpf:bb_get(offset,Offset),
	I is Offset+Pos,
	(
		recorded(I,current(A),_),
		writeln(A:t),
		fail;
		true
	),
	(
		recorded(I,current(A) ~=V,_),
		writeln(A:t ~= V),
		fail;
		true
	),
	(
		recorded(I,next(A),_),
		writeln(A:t+1),
		fail;
		true
	),
	(
		recorded(I,next(A)~=V,_),
		writeln(A:t+1 ~= V),
		fail;
		true
	),
	(
		recorded(I,A,_),
		A\=current(_),
		A\=current(_)~=_,
		A\=next(_),
		A\=next(_)~=_,
		writeln(A),
		fail;
		true
	).%,findall(A,(recorded(I,A,_), write(A),nl),_).

printkeyp(I) :-
	(
		recorded(I,current(A),_),
		writeln(A:t),
		fail;
		true
	),
	(
		recorded(I,current(A) ~=V,_),
		writeln(A:t ~= V),
		fail;
		true
	),
	(
		recorded(I,next(A),_),
		writeln(A:t+1),
		fail;
		true
	),
	(
		recorded(I,next(A)~=V,_),
		writeln(A:t+1 ~= V),
		fail;
		true
	),
	(
		recorded(I,A,_),
		A\=current(_),
		A\=current(_)~=_,
		A\=next(_),
		A\=next(_)~=_,
		writeln(A),
		fail;
		true
	).
	
sizep(Pos,Dim) :-
	dcpf:bb_get(offset,Offset),
	I is Offset+Pos,
	findall(A,recorded(I,A,_),L),
	length(L,Dim).
	
averagesizep(Particles,Mean) :-
	bb_put(sumobj,0.0),
	(
		between(1,Particles,Pos),
		sizep(Pos,Dim),
		bb_get(sumobj,OldTOT),
		NewTOT is OldTOT+Dim,
		bb_put(sumobj,NewTOT),
		fail;
		true
	),
	bb_delete(sumobj,T),
	Mean is T/Particles.
	
printkey(V) :-
	findall(A,(recorded(V,A,_), write(A),nl),_).
		
		


%%% particle filter %%%
init_particle(N) :-
	abolish_all_tables,
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	assert(user:timestep(-1)),
	bb_put(offset,0),
	bb_put(loglikelihood,0.0),
	magic,
	Tot is N*2+2,
	get_max_priority(MaxP),
	clean_sample(global),
	(
		between(1,Tot,I),
		clean_sample(I),
		bb_put(I,0.0),
		fail;
		true
	),
	(
		between(1,N,I),
		% NEW!
		(
			get_magic(true) ->
			(
				init_query(I,current(_)),
				init_query(I,current(_) ~= _)
			);
			true
		),
		generate_sample_particlefilter_prior(I,MaxP),
		%bb_put(I,1.0),
		fail;
		true
	),
	retractall(user:hardclause(prior(_),_,_)),
	retractall(user:distributionalclause(prior(_),_,_,_)).

% TO CHECK! init particle with the first observation
init_particle(PosEvidence,N) :-
	init_particle([],PosEvidence,N).
	
init_particle(Actions,PosEvidence,N) :-
	abolish_all_tables,
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	assert(user:timestep(-1)),
	bb_put(offset,0),
	bb_put(loglikelihood,0.0),
	magic,%_particlefilter,
	Tot is N*2+2,
	get_max_priority(MaxP),
	clean_sample(global),
	(
		between(1,Tot,I),
		clean_sample(I),
		bb_put(I,0.0),
		fail;
		true
	),
	Offset=0,
	%write('timestep '),write(0),nl,
	%bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		%write('particle '),write(I),nl,
		
		%(TO CHECK!! recently changed)
		(
			get_magic(true) ->
			(
				init_query(I,current(_)),
				init_query(I,current(_) ~= _),
				init_query_list(I,PosEvidence)
			)
			;
			true %init_query_list(I,ListProb)
		),
		assert_list(I,PosEvidence), % assert observations
		assert_list(I,Actions), % assert Actions
		generate_sample_particlefilter_prior(I,MaxP),
		fail;
		true
	),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		recorded(I,observation(_) ~= _,R),
		erase(R),
		fail;
		true
	),
	retractall(user:hardclause(prior(_),_,_)),
	retractall(user:distributionalclause(prior(_),_,_,_)).

get_logw(Pos,W) :-
	bb_get(offset,Offset),
	I is Offset+Pos,
	bb_get(I,W),!.

getwp(Pos,W) :-
	bb_get(offset,Offset),
	I is Offset+Pos,
	bb_get(I,LogW),
	W is exp(LogW),!.

get_particle_distribution(DistrNorm,N) :-
	bb_get(offset,Offset),
	bb_put(distrib,[]),
	bb_put(bestparticle,1),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		bb_get(I,WW),
		(
			WW>0.0 ->
			(
				bb_get(distrib,L),
				D=[(WW:I)|L],
				bb_put(distrib,D),
				
				bb_get(bestparticle,Best),
				Best1 is Offset+Best,
				bb_get(Best1,WBest),
				(
					WW>WBest ->
					bb_put(bestparticle,Pos);
					true
				)
			)
			;
			true
		),
		fail;
		true
	),
	bb_get(bestparticle,Best),
	Best1 is Offset+Best,
	bb_put(bestparticle,Best1),
	bb_get(Best1,WBest),
	%write(WBest),nl,
	bb_delete(distrib,Distribution),
	(
		Distribution=[] ->
		(
			nl,write('ERROR: all particles have weight 0!'),nl,
			% temporal solution: take the 1st particle
			Pos1 is Offset+1,
			DistrNorm=[1.0:Pos1]
		)
		;
		normalize(Distribution,DistrNorm)
	).%write(Distribution),nl,
	

get_logparticle_distribution(DistrNorm,N) :-
	bb_get(offset,Offset),
	bb_put(distrib,[]),
	bb_put(bestparticle,1),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		bb_get(I,WW),
		(
			WW>(-inf) ->
			(
				bb_get(bestparticle,Best),
				Best1 is Offset+Best,
				bb_get(Best1,WBest),
				(
					WW>WBest ->
					bb_put(bestparticle,Pos);
					true
				)
			)
			;
			true
		),
		fail;
		true
	),
	bb_get(bestparticle,Best),
	Best1 is Offset+Best,
	bb_put(bestparticle,Best1),
	bb_get(Best1,WBest),
	(
		WBest==(-inf) ->
		(
			writeln('Error: all particles have weight 0!'),writeln((WBest,Best)),
			(
				between(1,N,Pos1),
				I is Offset+Pos1,
				bb_put(I,0.0),
				fail;
				true
			)
		)
		;
		true
	),
	
	bb_put(distrib,[]),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		bb_get(I,WW),
		(
			exp(WW-WBest)>0 ->
			(
				bb_get(distrib,L),
				ExpW is exp(WW-WBest),
				D=[(ExpW:I)|L],
				bb_put(distrib,D)
			)
			;
			true
		),
		fail;
		true
	),


	bb_delete(distrib,Distribution),
	(
		Distribution=[] ->
		(
			nl,write('ERROR: all particles have weight 0!'),nl,
			% temporal solution: take the 1st particle
			Pos1 is Offset+1,
			DistrNorm=[1.0:Pos1]
		)
		;
		normalize(Distribution,DistrNorm)
	).%write(Distribution),nl,

% resampling
findpos(N,[],Cumulative,I) :-!.

findpos(N,[WW:Pos|T],Cumulative,I) :-
	bb_get(x,X),
	(
		(X=<N)
		->
		(
			Cum is Cumulative+WW,
			(
				X<Cum
				->
				(
					clean_sample(I),
					bb_put(I,0.0), % should it be log, i.e. 0, however it is normalized
					(
						recorded(Pos,CC,K),
						%write(CC),nl,
						(
							CC=next(Clause)
							->
								(
									recorda(I,current(Clause),_)%,
									%write((CC,current(Clause))),nl
								)
								;
								(
									CC = next(Clause2) ~= V2
									->
										recorda(I,current(Clause2) ~= V2,_)
									;
									(
										(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _ )
										->
											(
												recorda(I,CC,_)

											)
											;
											true
									)
								)
						),
						fail;
						true
					),
					%write(('pos ',Pos,' x ',X,' i ',I,' w ',WW)),nl,
					bb_get(bestparticle,Best),
					(
						Pos==Best ->
						bb_put(bestparticle,I);
						true
					),
					
					NewX is X+1/N,
					bb_put(x,NewX),
					NewI is I+1,
					findpos(N,[WW:Pos|T],Cumulative,NewI)
				);
				(
					findpos(N,T,Cum,I)
				)
			)
		)
		;
		true
	).


% resampling
findpos_aux(N,[],Cumulative,I) :-!.

findpos_aux(N,[WW:Pos|T],Cumulative,I) :-
	bb_get(x,X),
	(
		(X=<N)
		->
		(
			Cum is Cumulative+WW,
			(
				X<Cum
				->
				(
					clean_sample(I),
					bb_put(I,0.0),
					inferencestep_particlefilter_backward3(Pos),
					(
						recorded(Pos,CC,K),
						%write(CC),nl,
						(
							CC=next(Clause)
							->
								(
									recorda(I,current(Clause),_)%,
									%write((CC,current(Clause))),nl
								)
								;
								(
									CC = next(Clause2) ~= V2
									->
										recorda(I,current(Clause2) ~= V2,_)
									;
									(
										(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _ )
										->
											(
												recorda(I,CC,_)

											)
											;
											true
									)
								)
						),
						fail;
						true
					),
					%write(('pos ',Pos,' x ',X,' i ',I,' w ',WW)),nl,
					bb_get(bestparticle,Best),
					(
						Pos==Best ->
						bb_put(bestparticle,I);
						true
					),
					
					NewX is X+1/N,
					bb_put(x,NewX),
					NewI is I+1,
					findpos_aux(N,[WW:Pos|T],Cumulative,NewI)
				);
				(
					findpos_aux(N,T,Cum,I)
				)
			)
		)
		;
		true
	).

% Copy particle at position Old to New, where state_t+1 becomes state_t
copyparticles_core(Old,New) :-
	(
		recorded(Old,CC,K),
		%write(CC),nl,
		(
			CC=next(Clause)
			->
			(
				recorda(New,current(Clause),_)%,
				%write((CC,current(Clause))),nl
			)
			;
			(
				CC = next(Clause2) ~= V2
				->
					recorda(New,current(Clause2) ~= V2,_)
				;
				(
					(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
					->
					(
						recorda(New,CC,_)

					)
					;
						true
				)
			)
		),
		fail;
		true
	).

plaincopyparticles(Old,New) :-
	(
		recorded(Old,CC,K),
		%write(CC),nl,
		(
			CC=current(Clause)
			->
			(
				recorda(New,current(Clause),_)%,
				%write((CC,current(Clause))),nl
			)
			;
			(
				CC = current(Clause2) ~= V2
				->
					recorda(New,current(Clause2) ~= V2,_)
				;
				(
					(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
					->
					(
						recorda(New,CC,_)

					)
					;
						true
				)
			)
		),
		fail;
		true
	).
	
copyparticles(Distribution,Offset,N) :-
	(
		between(1,N,Index),
		I is Offset+Index,
		clean_sample(I),
		bb_put(I,(-inf)),
		fail;
		true
	),
	bb_get(bestparticle,Best),
	NewBest is Best-(N-Offset)+Offset,
	bb_put(bestparticle,NewBest),
	
	length(Distribution,Size),
	(
		between(1,Size,Index),
		nth1(Index,Distribution,WW:Pos),
		I is Offset+Index,
		(
			clean_sample(I),
			LogWW is log(WW),
			bb_put(I,LogWW),
			copyparticles_core(Pos,I)
			/*
			(
				recorded(Pos,CC,K),
				%write(CC),nl,
				(
					CC=next(Clause)
					->
						(
							recorda(I,current(Clause),_)%,
							%write((CC,current(Clause))),nl
						)
						;
						(
							CC = next(Clause2) ~= V2
							->
								recorda(I,current(Clause2) ~= V2,_)
							;
							(
								(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
								->
									(
										recorda(I,CC,_)

									)
									;
									true
							)
						)
				),
				fail;
				true
			)
*/
		),
		fail;
		true
	).

copyparticles_aux(Distribution,Offset,N) :-
	(
		between(1,N,Index),
		I is Offset+Index,
		clean_sample(I),
		bb_put(I,(-inf)),
		fail;
		true
	),
	bb_get(bestparticle,Best),
	NewBest is Best-(N-Offset)+Offset,
	bb_put(bestparticle,NewBest),
	
	length(Distribution,Size),
	(
		between(1,Size,Index),
		nth1(Index,Distribution,WW:Pos),
		I is Offset+Index,
		(
			clean_sample(I),
			LogWW is log(WW),
			bb_put(I,LogWW),
			inferencestep_particlefilter_backward3(Pos),
			copyparticles_core(Pos,I)
			/*
			(
				recorded(Pos,CC,K),
				%write(CC),nl,
				(
					CC=next(Clause)
					->
						(
							recorda(I,current(Clause),_)%,
							%write((CC,current(Clause))),nl
						)
						;
						(
							CC = next(Clause2) ~= V2
							->
								recorda(I,current(Clause2) ~= V2,_)
							;
							(
								(CC\=current(_), CC\= current(_) ~= _ , CC\=callmagic(_), CC\=temp(_), CC\= temp(_) ~= _, CC\=action(_), CC\=observation(_)~= _)
								->
									(
										recorda(I,CC,_)

									)
									;
									true
							)
						)
				),
				fail;
				true
			)
*/
		),
		fail;
		true
	).
% TO TEST (seems to work)
resampling(N) :-
	Rand is random/N,
	bb_get(offset,Offset),
	%get_particle_distribution(Distribution,N),
	get_logparticle_distribution(Distribution,N),
%	write(Distribution),nl,
	effectivenumparticles(Distribution,Neff),
	RelN is Neff/N*100,
%	write('effective particles '),write(RelN),nl,
	NewOffset is N-Offset,
	bb_put(offset,NewOffset),
	(
		Neff<N/2 ->
		(
			% resampling
			bb_put(x,Rand),
			I is NewOffset+1,
			findpos(N,Distribution,0,I)
		)
		;
		copyparticles(Distribution,NewOffset,N) % no resampling
	).

resampling_ps(N) :-
	Rand is random/N,
	bb_get(offset,Offset),
	%get_particle_distribution(Distribution,N),
	get_logparticle_distribution(Distribution,N),
	%write(Distribution),nl,
	effectivenumparticles(Distribution,Neff),
	RelN is Neff/N*100,
	write('effective particles '),write(RelN),nl,
	(
		Neff<N ->
		(
			write('resampling'),
			NewOffset is N-Offset,
			bb_put(offset,NewOffset),
			% resampling
			bb_put(x,Rand),
			I is NewOffset+1,
			findpos_ps(N,Distribution,0,I)
		)
		;
		true % no resampling
	).

findpos_ps(N,[],Cumulative,I) :-!.

findpos_ps(N,[WW:Pos|T],Cumulative,I) :-
	bb_get(x,X),
	(
		(X=<N)
		->
		(
			Cum is Cumulative+WW,
			(
				X<Cum
				->
				(
					clean_sample(I),
					bb_put(I,0.0),
					(
						recorded(Pos,CC,K),
						recorda(I,CC,_),
						fail;
						true
					),
					%write(('pos ',Pos,' x ',X,' i ',I,' w ',WW)),nl,
					bb_get(bestparticle,Best),
					(
						Pos==Best ->
						bb_put(bestparticle,I);
						true
					),
					
					NewX is X+1/N,
					bb_put(x,NewX),
					NewI is I+1,
					findpos_ps(N,[WW:Pos|T],Cumulative,NewI)
				);
				(
					findpos_ps(N,T,Cum,I)
				)
			)
		)
		;
		true
	).

		
eval_weight(I,[],1.0) :- !.

eval_weight(I,[H~=Val|T],P) :-
	eval_weight(I,T,PT),
	user:distributionalclause(next(H),D,Body,_),
	query_proof(I,Body),
	likelihood_weighting(Val,D,PH),
	%write((H,D,Body,Val,PH)),nl,
	P is PT*PH.



assert_list(Key,[]) :- !.
assert_list(Key,[H|T]) :-
	recorda(Key,H,_),
	assert_list(Key,T).

assert_list_next(Key,[]) :- !.
assert_list_next(Key,[H|T]) :-
	recorda(Key,H:t+1,_), % assert at time t+1
	assert_list_next(Key,T).

step_particle(PosEvidence,N) :-
	step_particle([],PosEvidence,[],N,1.0).

step_particle(Actions,PosEvidence,N) :-
	step_particle1(Actions,PosEvidence,[],N,1.0),
	resampling(N).

step_particle(Actions,PosEvidence,N,Delta) :-
	step_particle1(Actions,PosEvidence,[],N,Delta),
	resampling(N).

step_particle(Actions,PosEvidence,Constraints,N,Delta) :-
	step_particle1(Actions,PosEvidence,Constraints,N,Delta),
	resampling(N).

step_particle1(Actions,PosEvidence,Constraints,N,Delta) :-
	%statistics(cputime,[TimeInit,_]),
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	bb_get(offset,Offset),
	bb_put(likelihood,0.0),
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
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/2), % FOR TABLING
		%write('particle '),write(I),nl,
		assert_list(I,PosEvidence), % assert observations
		assert_list(I,Actions), % assert Actions
		
		%(TO CHECK!! recently changed)
		
		bb_get(I,LogTempWeight),
		TempWeight is exp(LogTempWeight),
		TempWeight>0.0,
		bb_get(initdcpf,InitDCPF),
		(
			get_magic(true) ->
			(
				init_query(I,next(_)),
				init_query(I,next(_) ~= _),
				init_query_list(I,PosEvidence)
			)
			;
			true %init_query_list(I,PosEvidence) % add callmagic for weight
		),
		
		(
			get_magic(backward) ->
			(
				user:raoblackwellisation(false) ->
				(	% Evaluate weight
					logweight_constraints(10,I,PosEvidence,Constraints,P)
					%eval_weight_backward(I,PosEvidence,P)%,
					%write('weight '),write(P),nl
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
					inferencestep_particlefilter_backward3(I)
				;
					generate_sample_particlefilter(I,MaxP)
			)
			;
			true
		),
		%dcpf:bb_get(offset,Offset),I is Offset+1,findall(A,(recorded(I,A,_),write(A),nl),_),trace,
		(
			user:raoblackwellisation(true) ->
			(
				% NEW
				add_rao_backward(I,PosEvidence),
				inferencestep_particlefilter_magicoff_rao(I,_),
				%generate_sample_particlefilter(I,MaxP)
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

					%findall(AA,(recorded(I,AA,_),write(AA),nl),_),
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
					
					),% For new variables: evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...) 
					(
						% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
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
					true %	get_magic(backward)
					
				)
			)
		),
		%write('weight '),write(P),nl,
		bb_get(I,OldWeight),
		NewW is OldWeight+P, % log
		bb_put(I,NewW),
		
		bb_get(likelihood,Likelihood),
		NewLikelihood is Likelihood+exp(NewW),
		bb_put(likelihood,NewLikelihood),
		%writeln(('Likelihood ',Likelihood,' NewW ',NewW,' NewLikelihood',NewLikelihood)),
		%bb_put(I,P),
		%write('Oldweight '),write(OldWeight),write(' weight '),write(NewW),nl,
		fail;
		true
	),
	bb_get(likelihood,TotLikelihood),
	bb_get(loglikelihood,LogLikelihood),
	NewLogLikelihood is LogLikelihood+log(TotLikelihood/N),
	bb_put(loglikelihood,NewLogLikelihood),
	%writeln(('TotLikelihood',TotLikelihood,' LogLikelihood ',LogLikelihood,'  NewLogLikelihood ',NewLogLikelihood)),
	(
		user:lifted(true) -> % lifted part
		(
			(distributionalclause:proof_query_backward_lifted(global,next(A)),fail;true),
			distributionalclause:current2next(global)
		)
		;
		true
	),
	bb_put(initdcpf,false).
	%statistics(cputime,[TimeEnd1,_]),
	%Tempo1 is (TimeEnd1-TimeInit)/1000.0,
	%write('before resampling '),write(Tempo1),nl,
	%printp(1),
	%resampling(N).%,
	%statistics(cputime,[TimeEnd2,_]),
	%Tempo2 is (TimeEnd2-TimeInit)/1000.0,
	%write(Tempo2),nl.

findbestaction(Episode,T,ActionList,[Bestaction]) :-
	bb_put(maxr,-100000000000),
	(
		between(1,5,Trials),
		sample(gaussian([0,0],[0.004,0,0,0.004]),(A,B)),
		evaluateaction(Episode,T,action(move(A,B)),NR),
		bb_get(maxr,MR),
		(
			NR>MR ->
			(
				bb_put(maxr,NR),
				bb_put(bestaction,action(move(A,B)))
			)
			;
			true
		),
		fail;
		true
	),
	bb_delete(bestaction,Bestaction),
	bb_delete(maxr,V),
	writeln(bestaction(Bestaction,V)).

evaluateaction(Episode,T,Action,NormR) :-
	Limit is Episode-1,
	bb_put(averagereward,0.0),
	bb_put(sumweight,0.0),
	(
		between(1,Limit,I),
		term_to_atom(p(T,I),Key), % stored episode
		term_to_atom(p(T,Episode),CurrentKey),
		recorda(CurrentKey,Action,_),
		writeln('--- Stored episode ---'),
		printkey(Key),
		writeln('--- Current episode ---'),
		printkey(CurrentKey),
		findall(Var~=Val,recorded(Key,next(Var) ~=Val,_),L),
		%writeln(list(L)),
		dcpf:eval_weight_planning(Key,CurrentKey,L,P),
		recorded(CurrentKey,Action,Ref),
		erase(Ref),
		writeln('--- New Current episode ---'),
		printkey(CurrentKey),
		recorded(Key,likelihood(Likelihood),_),
		bb_get(sumweight,OldSum),
		NewSum is OldSum + P/Likelihood,
		bb_put(sumweight,NewSum),
		
		recorded(Key,v(next,V),_),
		writeln(probV(P,V)),
		bb_get(averagereward,OldR),
		NewR is OldR+P/Likelihood*V,
		bb_put(averagereward,NewR),
		fail;
		true
	),
	bb_delete(averagereward,FinalR),
	bb_delete(sumweight,Sum),
	(
	Sum>0.0
	->
		NormR is FinalR/Sum
	;
		NormR is 0
	),
	writeln(NormR is FinalR/Sum).
%	
%	findall(Var~=Val,recorded(Key,next(Var) ~=Val,_),L),	
%	dcpf:eval_weight_planning(Key,L,P).
% planning using epsilon-greedy (page 120 RL 2012)

eval_weight_planning(Key1,Key2,[],1.0) :- !.

eval_weight_planning(Key1,Key2,[Var~=Val|T],P) :-
	eval_weight_planning(Key1,Key2,T,PT),
	user:distributionalclause(next(Var),D,Body,_),
	proof_query_backward(Key1,Key2,Body),!,
	likelihood_weighting(Val,D,PH),!,%writeln((H~=Val,PH)),
	P is PT*PH,!.

eval_weight_planning(Key,[],1.0) :- !.

eval_weight_planning(Key,[Var~=Val|T],P) :-
	eval_weight_planning(Key,T,PT),
	user:distributionalclause(next(Var),D,Body,_),
	proof_query_backward(Key,Body),!,
	likelihood_weighting(Val,D,PH),!,%writeln((H~=Val,PH)),
	P is PT*PH,!.
	
	
generate_episode(Episode,ActionList,0,Delta,0.0) :- !.

generate_episode(Episode,ActionList,Depth,Delta,TotalReward) :-
	Depth>0,
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
%	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	%write('timestep '),write(T),nl,
	abolish_all_tables,
	%write('particle '),write(I),nl,
	
	term_to_atom(p(Prec,Episode),KeyPrec),
	term_to_atom(p(T,Episode),KeyNext),
	findbestaction(Episode,Prec,ActionList,Action),
	assert_list(KeyPrec,Action), % assert Actions
	(
	get_magic(backward) ->
		inferencestep_particlefilter_backward2(KeyPrec)
	;
		writeln('error, only backward inference supported')%generate_sample_particlefilter(T,MaxP)
	),
	NewDepth is Depth-1,
	clean_sample(KeyNext),
	copyparticles_core(KeyPrec,KeyNext),
	proof_query_backward(KeyPrec,current(reward) ~= R),
	(
	proof_query_backward(KeyPrec,current(stop)) ->
		(
			TotalReward=R,
			V=0.0,
			Likelihood=1
		)
		;
		(
			generate_episode(Episode,ActionList,NewDepth,Delta,V),
			TotalReward is V+R,
			% compute likelihood p(next|current)
			findall(Var~=Val,recorded(KeyPrec,next(Var) ~=Val,_),L),
			eval_weight_planning(KeyPrec,L,Likelihood)
		)
	),
	
	removecurrent(KeyPrec),
	
	recorda(KeyPrec,v(current,TotalReward),_),
	recorda(KeyPrec,v(next,V),_),
	recorda(KeyPrec,likelihood(Likelihood),_),
	writeln((KeyPrec,v(current,TotalReward),v(next,V))).

start_sst(C,Depth,TotalReward) :-
	init_particle(1),
	dcpf:step_particle([],[observation(object(1)) ~= (0.6,0.89,0)],[],1,0.100000),plotdata(1),
	term_to_atom(p(0,1),Key),
	bb_get(dcpf:offset,Offset),I is Offset+1,
	plaincopyparticles(I,Key).

sst(C,T,S,Depth,TotalReward,BestAction) :-
	term_to_atom(p(T,S),Key),
	findall(action(move(A,B)),(between(1,2,_),sample(gaussian([0,0],[0.004,0,0,0.004]),(A,B))),ListActions),
	!,
	proof_query_backward(Key,current(reward) ~= R),
	(
		(proof_query_backward(Key,current(stop));Depth==0) ->
			(
				TotalReward=R
			)
			;
			(
				bb_put(maxr,-10000000000000),
				(
				member(Action,ListActions),
				bb_put(sumsst,0.0),
				(
					between(1,C,Sample),
					NextT is T+1,
					term_to_atom(p(NextT,Sample),KeyNew),
					
					(
						(
						clean_sample(tempsst),
						plaincopyparticles(Key,tempsst),
		
						retractall(user:deltaT(_)),
						assert(user:deltaT(Delta)),
						retractall(user:timestep(_)),
						assert(user:timestep(NextT)),
						abolish_all_tables,
						assert_list(tempsst,Action), % assert Actions
						(
						get_magic(backward) ->
							inferencestep_particlefilter_backward2(tempsst)
						;
							writeln('error, only backward inference supported')%generate_sample_particlefilter(T,MaxP)
						),
						NewDepth is Depth-1,
						clean_particle(tempsst),
						copyparticles_core(tempsst,KeyNew),
						sst(C,NextT,Sample,NewDepth,RewardS),
						bb_get(sumsst,SR),
						NewSR is SR + RewardS,
						bb_put(sumsst,NewSR)
						) -> true
					),
	
					fail;
					true
				),
				bb_get(sumsst,TotS),
				AvgR is TotS/C,
				bb_get(maxr,MaxR),
				(
					AvgR>MaxR ->
					(
						bb_put(maxr,AvgR),
						bb_put(maxaction,Action)
					)
					;
					true
				),
				fail;
				true
				),
				bb_delete(maxaction,BestAction),
				bb_delete(maxr,FutureR),
				TotalReward is R+FutureR
			)
	).

removecurrent(Old) :-
	(
		recorded(Old,Fact,K),
		(Fact=current(_);Fact=current(_)~=_;Fact=action(_)),
		erase(K),
		fail;
		true
	).

step_particle_aux(Actions,PosEvidence,Constraints,N,Delta) :-
	%statistics(cputime,[TimeInit,_]),
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	bb_get(offset,Offset),
	bb_put(likelihood,0.0),
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
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/2), % FOR TABLING
		%write('particle '),write(I),nl,
		assert_list(I,PosEvidence), % assert observations
		assert_list(I,Actions), % assert Actions
		
		%(TO CHECK!! recently changed)
		
		bb_get(I,LogTempWeight),
		TempWeight is exp(LogTempWeight),
		TempWeight>0.0,
		
		(
			get_magic(true) ->
			(
				init_query(I,next(_)),
				init_query(I,next(_) ~= _),
				init_query_list(I,PosEvidence)
			)
			;
			true %init_query_list(I,PosEvidence) % add callmagic for weight
		),
		
		(
			get_magic(backward) ->
			(
				user:raoblackwellisation(false) ->
				(
					logweight_constraints(10,I,PosEvidence,Constraints,P)
					%eval_weight_backward(I,PosEvidence,P)%,
					%write('weight '),write(P),nl
				)
				;
					true
			)
			;
			true
		),
/*		(
			get_magic(backward) ->
				inferencestep_particlefilter_backward3(I)
			;
				generate_sample_particlefilter(I,MaxP)
		),*/
		%write('weight '),write(P),nl,
		bb_get(I,OldWeight),
		NewW is OldWeight+P, % log
		bb_put(I,NewW),
		
		bb_get(likelihood,Likelihood),
		NewLikelihood is Likelihood+exp(NewW),
		bb_put(likelihood,NewLikelihood),
		%writeln(('Likelihood ',Likelihood,' NewW ',NewW,' NewLikelihood',NewLikelihood)),
		%bb_put(I,P),
		%write('Oldweight '),write(OldWeight),write(' weight '),write(NewW),nl,
		fail;
		true
	),
	bb_get(likelihood,TotLikelihood),
	bb_get(loglikelihood,LogLikelihood),
	NewLogLikelihood is LogLikelihood+log(TotLikelihood/N),
	bb_put(loglikelihood,NewLogLikelihood),
	%writeln(('TotLikelihood',TotLikelihood,' LogLikelihood ',LogLikelihood,'  NewLogLikelihood ',NewLogLikelihood)),
	(
		user:lifted(true) -> % lifted part
		(
			(distributionalclause:proof_query_backward_lifted(global,next(A)),fail;true),
			distributionalclause:current2next(global)
		)
		;
		true
	),
	Rand is random/N,
%	bb_get(offset,Offset),
	get_logparticle_distribution(Distribution,N),
%	write(Distribution),nl,
	effectivenumparticles(Distribution,Neff),
	RelN is Neff/N*100,
%	write('effective particles '),write(RelN),nl,
	NewOffset is N-Offset,
	bb_put(offset,NewOffset),

	(
		Neff<N/2 ->
		(
			% resampling
			bb_put(x,Rand),
			II is NewOffset+1,
			findpos_aux(N,Distribution,0,II)
		)
		;
		copyparticles_aux(Distribution,NewOffset,N) % no resampling
	).

reset_loglikelihood :-
	bb_put(loglikelihood,0.0).

get_loglikelihood(L) :-
	bb_get(loglikelihood,L).

% partitioned sampling
step_particle_ps(PosEvidence,N) :-
	step_particle_ps([],PosEvidence,N,1.0).

step_particle_ps(Actions,PosEvidence,N) :-
	step_particle_ps(Actions,PosEvidence,N,1.0).

step_particle_ps(Actions,PosEvidence,N,Delta) :-
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	bb_get(offset,Offset),
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		%write('particle '),write(I),nl,
		assert_list(I,PosEvidence), % assert observations
		assert_list(I,Actions),
		fail;
		true
	),
	step_particle_ps_cicle(Actions,PosEvidence,N,Delta).

step_particle_ps_cicle(Actions,[],N,Delta) :-
	step_particle(Actions,[],N,Delta),!.

step_particle_ps_cicle(Actions,[H|PosEvid],N,Delta) :-
	(PosEvid=[]) ->
	(
		step_particle(Actions,[H],N,Delta)
	);
	(
		bb_get(offset,Offset),
		get_max_priority(MaxP),
		bb_put(sample_sum,0.0),
		(
			between(1,N,Pos),
			I is Offset+Pos,
			abolish_all_tables,
			
			bb_get(I,LogTempWeight),
			TempWeight is exp(LogTempWeight),
			TempWeight>0.0,
			
			(
				get_magic(true) ->
				(
					init_query(I,next(_)),
					init_query(I,next(_) ~= _),
					init_query_list(I,[H])
				)
				;
				true %init_query_list(I,PosEvidence) % add callmagic for weight
			),
			
			(
				get_magic(backward) ->
				(
					user:raoblackwellisation(false) ->
					(
						eval_weight_backward(I,[H],P)%,
						%write('weight '),write(P),nl
					)
					;
						true
				)
				;
				true
			),/*
		(
			get_magic(backward) ->
				inferencestep_particlefilter_backward2(I)
			;
				generate_sample_particlefilter(I,MaxP)
		),*/
			%dcpf:bb_get(offset,Offset),I is Offset+1,findall(A,(recorded(I,A,_),write(A),nl),_),trace,
			(
				user:raoblackwellisation(true) ->
				(
					% NEW
					add_rao_backward(I,[H]),
					inferencestep_particlefilter_magicoff_rao(I,_),
					%generate_sample_particlefilter(I,MaxP)
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
					eval_weight(I,[H],P)
				;
				(
					user:raoblackwellisation(true) ->
					(
						nl,nl,write('particle '),write(I),nl,
	
						%findall(AA,(recorded(I,AA,_),write(AA),nl),_),
						findall(Sum,
							(
								recorded(I,next(VarRao) ~= finite(Distr),R),
								user:rao(VarRao),
								
								write(next(VarRao) ~= finite(Distr)),nl,
								findall(NewP:ValRao,(
											member(Pval:ValRao,Distr),
											
											eval_weightRao(I,[H],WRao,VarRao,ValRao),
											write(eval_weightR(I,[H],WRao,VarRao,ValRao)),nl,
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
							
							write((next(VarRao),kalman(A,B,Input,MeanSys,CovSys,H,MeanMeas,CovMeas),Body,_)),nl,
							recorded(I,current(VarRao) ~= gaussian(M,Cov),_),
							% if not recorded find the prior:
							% TO DO!!
							%
	
							(
								member(observation(VarRao)~=Vevidence,[H]) ->
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
						
						),% For new variables: evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...) 
						(
							% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
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
							member(observation(VarRao)~=Vevidence,[H]),
							
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
							eval_weight_magicoff(I,[H],P)
						)
						;
						true %	get_magic(backward)
						
					)
				)
			),
			%write('weight '),write(P),nl,
			bb_get(I,OldWeight),
			NewW is OldWeight+P, %log
			bb_put(I,NewW),%bb_put(I,P),
			%write('Oldweight '),write(OldWeight),write(' weight '),write(NewW),nl,
			fail;
			true
		),
		resampling_ps(N)
	),
	step_particle_ps_cicle(Actions,PosEvid,N,Delta).

effectivenumparticles(List,Neff) :-
	bb_put(sumweights,0.0),
	(
		member(WW:I,List),
		(
			WW>0.0 ->
			(
				bb_get(sumweights,Old),
				NewSum is Old+WW*WW,
				bb_put(sumweights,NewSum)
			)
			;
			true
		),
		fail;
		true
	),
	!,
	bb_delete(sumweights,S),
	Neff is 1/S.



eval_weight_magicoff(I,[],1.0) :- !.

eval_weight_magicoff(I,[H~=Val|T],P) :-
	eval_weight_magicoff(I,T,PT),
	%user:hardclause(weight(H,PH),Body,_),
	
	user:distributionalclause(next(H),D,Body,_),
	%write(H~=Val),write(' distr '),write(D),write(' Body '),write(Body),write('  Weight'),write(PH),nl,
	query_proof(I,Body),!,
	likelihood_weighting(Val,D,PH),
	%write(H~=Val),write(' distr '),write(D),write(' Body '),write(Body),write('  Weight'),write(PH),nl,
	P is PT*PH.

test_constraints(I,[],1.0) :- !.

test_constraints(I,[Var~=Val|T],P) :-
	test_constraints(I,T,PT),
	(
		(PT==0.0;PT==0) ->
			P=PT
		;
		(
			user:distributionalclause(next(Var),D,Body,_),
			proof_query_backward(I,Body),!,
			likelihood_weighting(Val,D,PH),!,
			P is PT*PH%,
			%writeln(test_constraints(P))
		)
	),!.

clearnext(I) :-
	(
		(
			recorded(I,next(TT),R),
			%writeln(TT),
			erase(R)
		);
		(
			recorded(I,next(TT) ~= VV,R),
			%writeln(TT~=VV),
			erase(R)
		),
		fail;
		true
	).
	
eval_weight_constraints(Iteration,I,O,C,P) :-
	eval_weight_backward(I,O,W),
	test_constraints(I,C,PC),!,
	%writeln(eval_weight_backward(I,O,W)),
	(
		(PC<1) ->
		(
			%writeln('repeat'),
			(
				Iteration>0 ->
				(
				Next is Iteration-1,
				
				%findall(next(object(ID))~=V,(recorded(I,next(object(ID))~=V,_),\+recorded(I,action(move(ID,_)),_),\+recorded(I,observation(object(ID))~=_,_) ),Variables),
				findall(next(object(ID))~=V,(recorded(I,next(object(ID))~=V,_),\+recorded(I,action(move(ID,_)),_) ),Variables),
				
				clearnext(I),
				mcmc(I,Variables,C,8),
				%writeln(eval_weight_constraints(Next,I,O,C,P)),
				eval_weight_constraints(Next,I,O,C,P)
				%writeln(P)
				)
				;
				(
					P=0,
					write('fail')
				)
			)
		)
		;
			P is W*PC
	),!.

logweight_constraints(Iteration,I,O,C,P) :-
	(
	logweight_backward(I,O,W) ->
		true;
		( write(logweight_backward(I,O,W)),writeln(' failed'),printkeyp(I) )
	),
	test_constraints(I,C,PC),!,
	%writeln(eval_weight_backward(I,O,W)),
	(
		(PC<1) ->
		(
			%writeln('repeat'),
			(
				Iteration>0 ->
				(
				Next is Iteration-1,
				
				%findall(next(object(ID))~=V,(recorded(I,next(object(ID))~=V,_),\+recorded(I,action(move(ID,_)),_),\+recorded(I,observation(object(ID))~=_,_) ),Variables),
				findall(next(object(ID))~=V,(recorded(I,next(object(ID))~=V,_),\+recorded(I,action(move(ID,_)),_) ),Variables),
				
				clearnext(I),
				mcmc(I,Variables,C,5),
				%writeln(eval_weight_constraints(Next,I,O,C,P)),
				logweight_constraints(Next,I,O,C,P)
				%writeln(P)
				)
				;
				(
					P=(-inf),
					write('fail')
				)
			)
		)
		;
			P is W+log(PC)
	),!.


indepcovariance(1,Var,[Var]) :- !.
indepcovariance(2,Var,[Var,0,0,Var]) :- !.	
indepcovariance(3,Var,[Var,  0,  0,
			   0,Var,  0,
			   0,  0,Var]) :- !.

mcmc_jump(I,[],Constraints,[]) :- !.

mcmc_jump(I,[H~=Val|T],Constraints,[H~=NewTuple|NT]) :-
	mcmc_jump(I,T,Constraints,NT),
%	proof_query_backward(I,H~=Val),
	test_to_list(Val,List),
	length(List,N),
	indepcovariance(N,0.0005,Cov),
	sample(gaussian(List,Cov),NewTuple).
/*
dcpf:mcmc_newsample(1,[next(object(5)),next(object(8))],[],N).
dcpf:bb_get(offset,Offset).

dcpf:mcmc(1,[next(object(5)),next(object(8))],[],W).
*/

erase_variables(I,[]) :- !.

erase_variables(I,[Var~=Val|T]) :-
	erase_variables(I,T),
	(
		(
		recorded(I,Var ~= Val,R),
		erase(R)
		)
		;
		true
	).

add_variables(I,[]) :- !.

add_variables(I,[Var~=Val|T]) :-
	add_variables(I,T),
	recorda(I,Var ~= Val,R).

mcmc_likelihood(I,Variables,Constraints,W) :-
	add_variables(I,Variables),
	test_constraints(I,Constraints,CW),
	erase_variables(I,Variables),
	test_to_list(TupleVariables,Variables),
	proof_query_backward_exp_eval(I,[],NV2,TupleVariables,WV),
	W is CW*WV,!.
/*
test

init_particle(1000).
dcpf:step_particle([action(move(8,(0.0,0.0,0.0)))],[observation(object(5)) ~= (0.2,0.0,0.0),observation(object(8)) ~= (0.4,0.0,0.0)],[],1000,0.400000).
plotdata2(1000).
dcpf:step_particle([action(move(8,(0.04,0.0,0.0)))],[observation(object(5)) ~= (0.2,0.0,0.0)],[],1000,0.400000).
plotdata2(1000).
dcpf:step_particle([action(move(8,(0.03,0.0,0.0)))],[observation(object(5)) ~= (0.22,0.0,0.0)],[],1000,0.400000).
plotdata2(1000).
dcpf:step_particle([action(move(8,(0.08,0.0,0.0)))],[observation(object(5)) ~= (0.3,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:step_particle([action(move(8,(0.05,0.0,0.0)))],[observation(object(5)) ~= (0.35,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:step_particle([action(move(8,(0.1,0.0,0.0)))],[observation(object(5)) ~= (0.45,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:step_particle([action(move(8,(0.1,0.0,0.0)))],[observation(object(5)) ~= (0.55,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:step_particle1([action(move(8,(0.1,0.0,0.0)))],[observation(object(5)) ~= (0.65,0.0,0.0)],[],1000,0.400000),plotdata2(1000).
dcpf:bb_get(offset,Offset).
dcpf:mcmc(1001,[next(object(5))~=A, next(object(8))~=B],[constraint(1)~= true],4).

*/
mcmc(I,Variables,Constraints,Steps) :-
	Steps=<0.0,
	!.

mcmc(I,Variables,Constraints,Steps) :-
%	write(Steps),
%	write(Variables),
	erase_variables(I,Variables),
	mcmc_likelihood(I,Variables,Constraints,OldW),
	mcmc_jump(I,Variables,Constraints,NewValues),
	mcmc_likelihood(I,NewValues,Constraints,NewW),
	(
	(NewW==0.0,OldW==0.0) ->
		Ratio=0.5
		;
		Ratio is NewW/OldW
	),
%	write(NewValues),
%	write(Ratio is NewW/OldW),
	(
	Ratio>=1 ->
		(
			NewStep is Steps-1,
			add_variables(I,NewValues),
			mcmc(I,NewValues,Constraints,NewStep)
		)
		;
		(
			Q is 1-Ratio,
			sample(finite([Ratio:true,Q:false]),Accept),	
			(
			Accept==true ->
			(
				NewStep is Steps-1,
				add_variables(I,NewValues),
				mcmc(I,NewValues,Constraints,NewStep)
			)
			;
			add_variables(I,Variables),
			NewStep2 is Steps-0.1,
			mcmc(I,Variables,Constraints,NewStep2)
			)
		)			
	),!.


	
eval_weight_backward(I,[],1.0) :- !.

eval_weight_backward(I,[Var~=Val|T],P) :-
	eval_weight_backward(I,T,PT),
	user:distributionalclause(next(Var),D,Body,_),
	(
	 (D=kalman2(H,MeanMeas,CovMeas)) ->
	 	(
	 		test_to_list(Val,ListVal),
			eval_weight_backward_kalman_simplified(I,Var~=ListVal,PH)
		)
	;
		(
			proof_query_backward(I,Body),!,
			likelihood_weighting(Val,D,PH)
			%writeln((Val,D,PH))
		)
	),!,%writeln((H~=Val,PH)),
	P is PT*PH.

logweight_backward(I,[],0.0) :- !.

logweight_backward(I,[Var~=Val|T],P) :-
	logweight_backward(I,T,PT),!,
	user:distributionalclause(next(Var),D,Body,_),
	(
	 (D=kalman2(H,MeanMeas,CovMeas)) ->
	 	(
	 		test_to_list(Val,ListVal),
			%eval_weight_backward_kalman_simplified(I,Var~=ListVal,PH)
			writeln('not implemented')
		)
	;
		(
			proof_query_backward(I,Body),!,
			log_likelihood_weighting(Val,D,PH)
			%writeln((Val,D,PH))
		)
	),!,%writeln((H~=Val,PH)),
	(
		%D=logfinite(_) ->
		P is PT+PH
		%;
		%P is PT+log(PH)
	).


% for kalman rao simplified NOT COMPLETE!
% the observation observation(Var) is related to the state next(Var) 
eval_weight_backward_kalman_simplified(I,observation(Var)~=Vevidence,P) :-
	%eval_weight_backward(I,T,PT),
	user:distributionalclause(next(observation(Var)),kalman2(H,MeanMeas,CovMeas),Body,_),
	proof_query_backward(I,Body),!,
	
	(
	recorded(I,next(Var) ~= Value,_) ->
		true
		;
		(
			%likelihood_weighting(Val,D,PH),
			user:distributionalclause(next(Var),Distribution,Body2,_),
			proof_query_backward(I,Body2),
			
			ground(Var), 
			ground(Distribution) %,
			%writeln(distributionalclause(next(Var),Distribution,Body2,_))
			%\+recorded(Key,next(Head2) ~= X,_)
		)
	),
	Value=distribution(D), % not implemented when Value is not a distribution 
	(
		%Distribution=gaussian(M,Cov),
		convertgaussian(Distribution,gaussian(M,Cov)),
		kalmanrao_simplified(M,Cov,H,MeanMeas,CovMeas,Vevidence,Mpost,CovPost,P), % there is no prediction step.
		%writeln(kalmanrao_simplified(M,Cov,H,MeanMeas,CovMeas,Vevidence,Mpost,CovPost,P)),
		recorda(I,next(Var) ~= distribution(gaussian(Mpost,CovPost)),_)
		%writeln(next(Var) ~= distribution(gaussian(Mpost,CovPost)))
	).
	%P is PT*PH.
	
% for kalman rao simplified NOT COMPLETE!
convertgaussian(indepGaussians([([M11,M12],[Cov11,Cov12,Cov21,Cov22]),([M21,M22],[Cov211,Cov212,Cov221,Cov222])]),
		gaussian([M11,M12,M21,M22],[Cov11,Cov12,    0,    0,
			  		    Cov21,Cov22,    0,    0,
			  		        0,    0,Cov211,Cov212,
			  		        0,    0,Cov221,Cov222])) :- !.
			  		        
convertgaussian(indepGaussians([([M11,M12],[Cov11,Cov12,Cov21,Cov22]),([M21,M22],[Cov211,Cov212,Cov221,Cov222]),([M31,M32],[Cov311,Cov312,Cov321,Cov322])]),
		gaussian([M11,M12,M21,M22,M31,M32],[Cov11,Cov12,    0,    0,	0,	0,
			  		    	    Cov21,Cov22,    0,    0,	0,	0,
			  		        	0,    0,Cov211,Cov212,	0,	0,
			  		        	0,    0,Cov221,Cov222,	0,	0,
			  		        	0,    0,     0,	    0,Cov311,Cov312,
			  		        	0,    0,     0,	    0,Cov321,Cov322])) :- !.


convertgaussian(indepGaussians([([M1],[Cov1]),([M2],[Cov2]),([M3],[Cov3])]),
		gaussian([M1,M2,M3],[Cov1,   0,   0,
					0,Cov2,   0,
					0,   0,Cov3])) :- !.	
							  		        			  		        
convertgaussian(gaussian(M,C),gaussian(M,C)) :- !.

convertgaussian(uniform(A),uniform(A)) :- !.

/*
convertgaussian(indepGaussians([G1,G2|T]),gaussian(M,C)) :-
	convertgaussian(indepGaussians([G1,G2]),gaussian(Mt,Ct)),
	convertgaussian(indepGaussians([(Mt,Ct)|T]),gaussian(M,C)).
*/
% add rao variables needed to evaluate the evidence 
add_rao_backward(I,[]) :- !.

add_rao_backward(I,[H~=Val|T]) :-
	user:distributionalclause(next(H),D,Body,_),
	query_proof_defineRaoBackward(I,Body),
	add_rao_backward(I,T),!.

% TO CHECK!
add_rao_backward(I,[observation(H)~=Val|T]) :-
	user:distributionalclause(next(H),kalman(_,_,_,_,_,_,_,_),Body,_),
	query_proof_defineRaoBackward(I,Body),
	add_rao_backward(I,T),!.
	
% evaluation weight with rao, p(z_t | a_t, b(i)_t) for a_t (Var) = Val
eval_weightRao(I,[],1.0,Var,Val) :- !.

eval_weightRao(I,[H~=Vevidence|T],P,Var,Val) :-
	eval_weightRao(I,T,PT,Var,Val),
	%user:hardclause(weight(H,PH),Body,_),
	user:distributionalclause(next(H),D,Body,_),
	(
		get_magic(backward) ->
		(
			query_proof_setRaoBackward(I,Body,Var,Val,Ignore),
			likelihood_weighting(Vevidence,D,PH),
			(
			Ignore==0 ->
				P is PT*PH
			;
				P=PT
			)
		)
		;
		(
			query_proof_setRao(I,Body,Var,Val,Ignore),
			likelihood_weighting(Vevidence,D,PH),
			(
			Ignore==0 ->
				P is PT*PH
			;
				P=PT
			)
		)
	),
	!.


% generate sample handling priority for particle filter
generate_sample_particlefilter(Key,MaxP) :-
	(
		between(0,MaxP,Priority),
		(
			get_magic(true) ->
				inferencestep_particlefilter(Key,Priority,F)
			;
			(
				get_magic(false) ->
				inferencestep_particlefilter_magicoff(Key,Priority,F)
				;
				(
					get_magic(backward) ->
						inferencestep_particlefilter_backward(Key,Priority,F)
					;
						( write('Inference method not selected!'),nl,fail )
				)
			)
		),
		fail;
		true
	).
	
generate_sample_particlefilter_prior(Key,MaxP) :-
	(
		between(0,MaxP,Priority),
		(
			get_magic(true) ->
				inferencestep_particlefilter(Key,Priority,F) % NOT TESTED!
			;
			(
				get_magic(backward) ->
				(
					inferencestep_particlefilter_backward_prior(Key,Priority,F),
					(
						recorded(Key,prior(A),R),
						recorda(Key,current(A),_),
						erase(R),
						fail;
						true
					),
					(
						recorded(Key,prior(A) ~= Val,R),
						recorda(Key,current(A) ~= Val,_),
						erase(R),
						fail;
						true
					)
				)
				;
					inferencestep_particlefilter_magicoff_prior(Key,Priority,F)
			)
		),
		fail;
		true
	).


% for get_magic(true)
inferencestep_particlefilter(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		query_proof(Key,Body),
		\+derived(Key,Head),
		ground(Head),
		%\+recorded(Key,Head,_),
		/*
		(
			Head\=callmagic(_) ->
				ground(Head)
			;
				true
		),
		*/
		recorda(Key,Head,_),
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(Head2,Distribution,Body,Pr),
		
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,Head2 ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,Head2 ~= Val,_),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

	
% for get_magic(false)
inferencestep_particlefilter_magicoff(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		query_proof(Key,Body),
		ground(Head),
		\+recorded(Key,Head,_),
		recorda(Key,Head,_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(Head2,Distribution,Body,Pr),
		Head2\=next(observation(_)),
		%Head2\=bk(_),
		
		% RAO
		(
			user:raoblackwellisation(true) ->
			(
				
				(Head2=current(Vr);Head2=next(Vr))  ->
				(
					\+user:rao(Vr)
				)
				;
				true
				
			)
			;
			true
		),
		
		%
		query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,Head2 ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,Head2 ~= Val,_),
		
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_magicoff(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

% for get_magic(backward)
inferencestep_particlefilter_backward(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(Head,Body,Pr),
		%Head\=_:0,
		proof_query_backward(Key,Body),%query_proof(Key,Body),
		ground(Head),
		\+recorded(Key,Head,_),
		recorda(Key,Head,_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(Head2,Distribution,Body,Pr),
		Head2\=next(observation(_)),
		%Head2\=_:0,
		% RAO
		(
			user:raoblackwellisation(true) ->
			(
				
				(Head2=current(Vr);Head2=next(Vr)) ->
				(
					\+user:rao(Vr)
				)
				;
				true
				
			)
			;
			true
		),
		%
		proof_query_backward(Key,Body),%query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,Head2 ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,Head2 ~= Val,_),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_backward(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

inferencestep_particlefilter_backward2(Key) :-
	bb_put(flag,false),
	(
		user:hardclause(next(Head),Body,_),
		proof_query_backward(Key,Body),
		ground(Head),
		\+recorded(Key,next(Head),_),
		recorda(Key,next(Head),_),
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		user:distributionalclause(next(Head2),Distribution,Body,_),
		Head2\=observation(_),
		% RAO
		(
			user:raoblackwellisation(true) ->
			(
				\+user:rao(Head2)				
			)
			;
			true
		),
		%
		proof_query_backward(Key,Body),%query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,next(Head2) ~= X,_),
		
		sample(Distribution,Val),
				
		recorda(Key,next(Head2) ~= Val,_),
		bb_put(flag,true),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
			inferencestep_particlefilter_backward2(Key)
		;
			true
	).


% inference next step keeping the distribution: recorda(Key,next(Head2) ~= Distribution,_)
inferencestep_particlefilter_backward3(Key) :-
	bb_put(flag,false),
	(
		user:hardclause(next(Head),Body,_),
		proof_query_backward(Key,Body),
		(
			ground(Head) ->
			true;
			writeln(notground(Head))
		),
		\+recorded(Key,next(Head),_),
		recorda(Key,next(Head),_),
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		user:distributionalclause(next(Head2),Distribution,Body,_),
		Head2\=observation(_),
		% RAO
		/*
		(
			user:raoblackwellisation(true) ->
			(
				\+user:rao(Head2)				
			)
			;
			true
		),
		*/
		%
		proof_query_backward(Key,Body),%query_proof(Key,Body),
		
		ground(Head2), 
		ground(Distribution), 
		\+recorded(Key,next(Head2) ~= X,_),
		
		%sample(Distribution,Val),
		
		recorda(Key,next(Head2) ~= distribution(Distribution),_),
		bb_put(flag,true),
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
			inferencestep_particlefilter_backward3(Key)
		;
			true
	).

% for get_magic(backward) init prior
inferencestep_particlefilter_backward_prior(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(prior(Head),Body,Pr),
		proof_query_backward(Key,Body),
		ground(Head),
		\+recorded(Key,prior(Head),_),
		recorda(Key,prior(Head),_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(prior(Head2),Distribution,Body,Pr),
		% RAO
		(
			user:raoblackwellisation(true) ->
			(
				
				user:rao(Head2) ->
				(
					query_proof_rao(Key,Body,_),
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,prior(Head2) ~= _,_),
					recorda(Key,prior(Head2) ~= Distribution,_)
				)
				;
				(
					proof_query_backward(Key,Body),
		
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,prior(Head2) ~= X,_),
		
					sample(Distribution,Val),
				
					recorda(Key,prior(Head2) ~= Val,_)
				)
				
			)
			;
			(
				proof_query_backward(Key,Body),
	
				ground(Head2), 
				ground(Distribution),
				\+recorded(Key,prior(Head2) ~= X,_),
	
				sample(Distribution,Val),
			
				recorda(Key,prior(Head2) ~= Val,_)
			)
		),
		%
		
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_backward_prior(Key,Pr,_)
		);
		(
			Flag=false
		)
	),
	(
		user:lifted(true) -> % Lifted part
		(
			user:distributionalclause(prior(Head2),Distribution,Body,Pr),
			(
					\+(( ground(Head2), ground(Distribution), ground(Body)) ),
					\+recorded(global,distributionalclause(current(Head2),_,Body,_),_),
					recorda(global,distributionalclause(current(Head2),Distribution,Body,0),_)
			),
			fail;
			true
		);
		true
	).
	

	
% for get_magic(false) init prior
inferencestep_particlefilter_magicoff_prior(Key,Pr,Flag) :-
	bb_put(flag,false),
	(
		%between(0,Priority,Pr),
		user:hardclause(prior(Head),Body,Pr),
		query_proof(Key,Body),
		ground(Head),
		\+recorded(Key,current(Head),_),
		recorda(Key,current(Head),_),
		
		%write(Head),nl,
		bb_put(flag,true),
		fail;
		
		true
	),
	%write('-------'),nl,
	(
		%between(0,Priority,Pr), 
		user:distributionalclause(prior(Head2),Distribution,Body,Pr),
		% RAO
		(
			user:raoblackwellisation(true) ->
			(
				
				user:rao(Head2) ->
				(
					query_proof_rao(Key,Body,_),
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,current(Head2) ~= _,_),
					recorda(Key,current(Head2) ~= Distribution,_)
				)
				;
				(
					query_proof(Key,Body),
		
					ground(Head2), 
					ground(Distribution),
					\+recorded(Key,current(Head2) ~= X,_),
		
					sample(Distribution,Val),
				
					recorda(Key,current(Head2) ~= Val,_)
				)
				
			)
			;
			(
				query_proof(Key,Body),
	
				ground(Head2), 
				ground(Distribution),
				\+recorded(Key,current(Head2) ~= X,_),
	
				sample(Distribution,Val),
			
				recorda(Key,current(Head2) ~= Val,_)
			)
		),
		%
		
		%write(Head2 ~= Val),write(' <- '),write(Pr),write(' '),write(Body),nl, % debug
		bb_put(flag,true),
		fail;
		
		true
	),
	bb_delete(flag,Value),
	(
		Value==true
		->
		(
			Flag=true,
			inferencestep_particlefilter_magicoff_prior(Key,Pr,_)
		);
		(
			Flag=false
		)
	).

% for rao-blackwellised magic=off,stratification not used for rao vars.
inferencestep_particlefilter_magicoff_rao(Key,_) :-
	/*
	(
		user:distributionalclause(current(VarRao),Distribution,Body,Priority),
		user:rao(VarRao),
		query_proof_rao(Key,Body,Weight),
		ground(VarRao), 
		ground(Distribution),
		\+recorded(Key,current(VarRao) ~= _,_),
		recorda(Key,current(VarRao) ~= Distribution,_),
		fail;
	
		true
	),
	*/
	
	% evaluation of clauses current() for rao variables NOT REALLY USED, TO CHECK!
	/*
	(
		
		user:distributionalclause(current(VarRao),Distribution,Body,_),
		user:rao(VarRao),
		
		proof_query_backward(Key,Body),
		

		ground(VarRao),
		ground(Distribution),
		%trace,
		%write(VarRao),nl,write(' Distribution '),write(Distribution),nl,
		\+recorded(Key,current(VarRao) ~= _,_),
		%write('new'),nl,
		recorda(Key,current(VarRao) ~= Distribution,_), % TO CHECK! is not current??
		%write(next(VarRao) ~= Distribution),nl,
		fail;
	
		true
		
	),*/
	%nl,write(Key),nl,
	%user:rao(VarRao1),
	%recorded(Key,current(VarRao1) ~= CurrentDistr,_),
	%write(current(VarRao1) ~= CurrentDistr),nl,
	(% evaluation of clauses next() for rao variables with a discrete distribution: finite(...)
		
		
		user:distributionalclause(next(VarRao),finite(Distribution),Body,_),
		user:rao(VarRao),
		%trace,
		query_proof_rao(Key,Body,Weight), % evaluated for every value that satisfy the body (backtracking)
		ground(VarRao),
		ground(Distribution),
		
		%write((Body,Weight)),nl,
		%write(VarRao),nl,write(' Distribution '),write(finite(Distribution)),nl,
		(
			recorded(Key,next(VarRao) ~= OldDistr,R) ->
			(
				sum_distrib(OldDistr,finite(Distribution),Weight,finite(NewDist)), % NewDist = OldDistr + Distribution * Weight
				%write(next(VarRao) ~= finite(NewDist)),nl,
				erase(R),
				cleanDistribution(NewDist,CleanedDistr,0.0),
				%write(next(VarRao) ~= finite(CleanedDistr)),nl,
				recorda(Key,next(VarRao) ~= finite(CleanedDistr),_)
			)
			;
			(
				%write('new'),nl,
				finite(Temp)=finite(Distribution),
				multiplyby(Temp,Weight,NewD),
				recorda(Key,next(VarRao) ~= finite(NewD),_)
				%write(next(VarRao) ~= finite(NewD)),nl
			)
		),
		
		fail;
	
		true
	).
	% add weight evaluation to add clauses from bk!.

% 

% bayesfilter TO CHECK!

init_bayesfilter :-
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	assert(user:timestep(-1)),
	magic,
	clean_sample(bayes),
	clean_sample(bayes2),
	init_query(bayes,current(_,_,_)).
	
bayesfilter_weight([],1.0) :- !.

bayesfilter_weight([H|T],P) :-
	bayesfilter_weight(T,PT),
	query_proof(I,weight(H,PH)),
	P is PT*PH.
	
list_weight([],[]) :-!.
list_weight([H|T],[weight(H,_,_,_)|T2]) :-
	list_weight(T,T2).


	
eval_query_particle(Q,N,P) :-
	timesyntax(Q,Query),
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/3), % FOR TABLING
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				get_magic(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				user:raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					get_magic(backward) ->
					(
						eraseall(tempparticle),
						%W=1, % error in proof_query_backward_eval
						%proof_query_backward_exp_eval(I,[],NV2,Query,W)% proof_query_backward(I,tempparticle,Query) %proof_query_backward_eval(I,tempparticle,Query,W)
						proof_query_backward_eval(I,tempparticle,Query,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
					)
					;
					(
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	P is SUCC/TOT,
	!.

eval_query_particle_alternative(Q,N,P) :-
	timesyntax(Q,Query),
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/3), % FOR TABLING
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				get_magic(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				user:raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					get_magic(backward) ->
					(
						eraseall(tempparticle),
						%W=1, % error in proof_query_backward_eval
						proof_query_backward_exp_eval(I,[],NV2,Query,W)% proof_query_backward(I,tempparticle,Query) %proof_query_backward_eval(I,tempparticle,Query,W)
						%proof_query_backward_eval(I,tempparticle,Query,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
					)
					;
					(
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	P is SUCC/TOT,
	!.

eval_query_particle_alternative2(Q,N,P) :-
	timesyntax(Q,Query),
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,0.0),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,%abolish_table(distributionalclause:proof_query_backward/3), % FOR TABLING
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				get_magic(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				user:raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					get_magic(backward) ->
					(
						eraseall(tempparticle),
						W=1, % error in proof_query_backward_eval
						proof_query_backward(I,tempparticle,Query) %proof_query_backward_exp_eval(I,[],NV2,Query,W)% proof_query_backward(I,tempparticle,Query) %proof_query_backward_eval(I,tempparticle,Query,W)
						%proof_query_backward_eval(I,tempparticle,Query,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
					)
					;
					(
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	P is SUCC/TOT,
	!.
	
% non-ground query like right(g,X) IMPLEMENTED ONLY FOR BACKWARD!
% output distribution of each X
eval_query_particle2(X,Q,N,P) :-
	%timesyntax(Q,Query),
	Q=Query,
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,[]),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				get_magic(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				% NOT IMPLEMENTED. old code
				user:raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					get_magic(backward) ->
					(
						eraseall(tempparticle),
						%findall(W:X,(proof_query_backward_exp_eval(I,[],NV2,Query,W)),List)
						findall(1:X,(distributionalclause:proof_query_backward(I,tempparticle,Query)),List)
						
						%findall(1:X,(distributionalclause:proof_query_backward_exp(I,[],NV2,Query)),List)
						->
						(
							%writeln(List),
							bb_get(succeeding_sample_sum,Old),
							sum_distrib(finite(Old),finite(List),Weight,finite(New)),
							%write((List,Weight,New)),nl,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
					)
					;
					( % NOT IMPLEMENTED. old code
						findall(1:X,query_proof(I,Query),List)
						->
						(
							bb_get(succeeding_sample_sum,Old),
							sum_distrib(finite(Old),finite(List),Weight,finite(New)),
							%write((List,Weight,New)),nl,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		P=[]
		;
		divideby(SUCC,TOT,P)
	),
	%P is SUCC/TOT,
	!.
	


eval_query_particle3(T,X,Q,N,P) :-
	writeln(n11:N),
	Q=Query,
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	%writeln(Offset),
	bb_put(succeeding_sample_sum,[]),
	bb_put(sample_sum,0.0),
	length(FinalL,N),
	(
		between(1,N,Pos),
		writeln(N),
		I is Offset+Pos,
		
		abolish_all_tables,
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		writeln(Weight),
		Weight>0,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		writeln(I),
		(
			(
				get_magic(true) ->
				(
					writeln(test1),
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			(
				user:raoblackwellisation(true) ->
				(
					writeln(test2),
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					(
						SumWW>0 ->
						(
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					get_magic(backward) ->
					(
						eraseall(tempparticle),
						writeln(I),
						writeln(Query),
						findall(1:X,(distributionalclause:proof_query_backward(I,tempparticle,Query)),List)
						->
						(
							writeln(ff:List),
							I2 is I-Offset,	
							assert(part(T,I2,List)),
							
							bb_get(succeeding_sample_sum,Old),
							sum_distrib(finite(Old),finite(List),Weight,finite(New)),
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
					)
					;
					( 
						findall(1:X,query_proof(I,Query),List)
						->
						(
							bb_get(succeeding_sample_sum,Old),
							sum_distrib(finite(Old),finite(List),Weight,finite(New)),
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	(
		SUCC==[] ->
		P=[]
		;
		divideby(SUCC,TOT,P)
	),
	findall((T,I2,List),part(T,I2,List),List2),
	%writeln(List2),
	!.

step_particle3(X,Q,N) :-

	step_particle3(X,Q,[],N,1.0),
	resampling(N).

step_particle3(X,Query,Constraints,N,Delta) :-
	%statistics(cputime,[TimeInit,_]),
	retractall(user:deltaT(_)),
	assert(user:deltaT(Delta)),
	bb_get(offset,Offset),
	%writeln(offset2:Offset),
	bb_put(likelihood,0.0),
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
		abolish_all_tables,
		eraseall(tempparticle),
		( X == [] ->
			(distributionalclause:proof_query_backward(I,tempparticle,Query) ->
			List = [ok]
			;
			List = []
			)
		;
			findall(X,(distributionalclause:proof_query_backward(I,tempparticle,Query)),List)

		),
		length(List,LengthLL),
		%assert_list(I,Actions), % assert Actions
	
		(  LengthLL == 0 ->
			%PosEvidence2 = [exists ~= false],
			PosEvidence2 = [observation(exists)~=false],			
			%write(I),write(' '),writeln(PosEvidence2),
			assert_list(I,PosEvidence2) % assert observations
		;
			%PosEvidence2 = [exists ~= true],
			PosEvidence2 = [observation(exists)~=true],
			%write(I),write(' '),writeln(PosEvidence2),
			assert_list(I,PosEvidence2)
		),
		
		
		%(TO CHECK!! recently changed)
		
		bb_get(I,LogTempWeight),
		TempWeight is exp(LogTempWeight),
		TempWeight>0.0,
		bb_get(initdcpf,InitDCPF),
		(
			get_magic(true) ->
			(
				init_query(I,next(_)),
				init_query(I,next(_) ~= _),
				init_query_list(I,PosEvidence2)
			)
			;
			true %init_query_list(I,PosEvidence) % add callmagic for weight
		),
		
		(
			get_magic(backward) ->
			(
				user:raoblackwellisation(false) ->
				(	% Evaluate weight
					logweight_constraints(10,I,PosEvidence2,Constraints,P)%TODO
					%eval_weight_backward(I,PosEvidence,P)%,
					%write('weight '),write(P),nl
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
					inferencestep_particlefilter_backward3(I)
				;
					generate_sample_particlefilter(I,MaxP)
			)
			;
			true
		),
		%dcpf:bb_get(offset,Offset),I is Offset+1,findall(A,(recorded(I,A,_),write(A),nl),_),trace,
		(
			user:raoblackwellisation(true) ->
			(
				% NEW
				add_rao_backward(I,PosEvidence2),
				inferencestep_particlefilter_magicoff_rao(I,_),
				%generate_sample_particlefilter(I,MaxP)
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
				eval_weight(I,PosEvidence2,P)
			;
			(
				user:raoblackwellisation(true) ->
				(
					nl,nl,write('particle '),write(I),nl,

					%findall(AA,(recorded(I,AA,_),write(AA),nl),_),
					findall(Sum,
						(
							recorded(I,next(VarRao) ~= finite(Distr),R),
							user:rao(VarRao),
							
							write(next(VarRao) ~= finite(Distr)),nl,
							findall(NewP:ValRao,(
										member(Pval:ValRao,Distr),
										
										eval_weightRao(I,PosEvidence2,WRao,VarRao,ValRao),
										write(eval_weightR(I,PosEvidence2,WRao,VarRao,ValRao)),nl,
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
							member(observation(VarRao)~=Vevidence,PosEvidence2) ->
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
					
					),% For new variables: evaluation of clauses next() for rao variables with a gaussian distribution: gaussian(...) 
					(
						% mean_Xt,cov_Xt,A,B,mean_sistem_uncertainty,cov_sistem_uncertainty,H,mean_measurement_uncertainty,cov_measurement_uncertainty,input,measurement,post mean,post cov
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
						member(observation(VarRao)~=Vevidence,PosEvidence2),
						
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
						eval_weight_magicoff(I,PosEvidence2,P)
					)
					;
					true %	get_magic(backward)
					
				)
			)
		),
		%write('weight '),write(P),nl,
		bb_get(I,OldWeight),
		NewW is OldWeight+P, % log

		bb_put(I,NewW),
		
		bb_get(likelihood,Likelihood),
		NewLikelihood is Likelihood+exp(NewW),
		bb_put(likelihood,NewLikelihood),
		%writeln(('Likelihood ',Likelihood,' NewW ',NewW,' NewLikelihood',NewLikelihood)),
		%bb_put(I,P),
		%write('Oldweight '),write(OldWeight),write(' weight '),write(NewW),nl,
		fail;
		true
	),
	bb_get(likelihood,TotLikelihood),
	bb_get(loglikelihood,LogLikelihood),
	NewLogLikelihood is LogLikelihood+log(TotLikelihood/N),
	bb_put(loglikelihood,NewLogLikelihood),
	%writeln(('TotLikelihood',TotLikelihood,' LogLikelihood ',LogLikelihood,'  NewLogLikelihood ',NewLogLikelihood)),
	(
		user:lifted(true) -> % lifted part
		(
			(distributionalclause:proof_query_backward_lifted(global,next(A)),fail;true),
			distributionalclause:current2next(global)
		)
		;
		true
	),
	bb_put(initdcpf,false).
	%statistics(cputime,[TimeEnd1,_]),
	%Tempo1 is (TimeEnd1-TimeInit)/1000.0,
	%write('before resampling '),write(Tempo1),nl,
	%printp(1),
	%resampling(N).%,
	%statistics(cputime,[TimeEnd2,_]),
	%Tempo2 is (TimeEnd2-TimeInit)/1000.0,
	%write(Tempo2),nl.

eval_average_particle(Avg,X,Query,N,P) :-
	get_max_priority(MaxP),
	bb_get(offset,Offset),
	bb_put(succeeding_sample_sum,[]),
	bb_put(sample_count,[]),
	bb_put(sample_sum,0.0),
	(
		between(1,N,Pos),
		I is Offset+Pos,
		abolish_all_tables,
		% TO CHECK recently removed
		bb_get(I,LogWeight),
		Weight is exp(LogWeight),
		Weight>0,
		%
		%Weight=1,
		bb_get(sample_sum,OldTOT),
		NewTOT is OldTOT+Weight,
		bb_put(sample_sum,NewTOT),
		(
			(
			
			% recently added TO CHECK (sample and inference to evaluate the query)
				get_magic(true) ->
				(
					init_query(I,Query),
					generate_sample_particlefilter(I,MaxP)
				)
				;
				true
			),
			%
			(
				% NOT IMPLEMENTED. old code
				user:raoblackwellisation(true) ->
				(
					% find all cases that satisfy the Query, then sum their weights
					findall(WW,query_proof_rao(I,Query,WW),ListWW),
					sum_list(ListWW,SumWW),
					%write('ListWW '),write(ListWW),nl,
					%write('SumWW '),write(SumWW),nl,
					(
						SumWW>0 ->
						(
							%write('Query '),write(Query),write(' WW '),write(SumWW),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+SumWW*Weight,% TO CHECK recently modified
							bb_put(succeeding_sample_sum,New)
						)
						;
						true
					)
				)
				;
				(
					get_magic(backward) ->
					(
						eraseall(tempparticle),
						findall(WVal:X,(proof_query_backward(I,tempparticle,Query~=Val),prod_scalar_multidim(Val,Weight,WVal)),List)
						->
						(
							findall(Weight:X,proof_query_backward(I,tempparticle,Query~=Val),Counts),
							bb_get(sample_count,OldCounts),
							sum_list_multidim(OldCounts,Counts,NewCounts),
							bb_put(sample_count,NewCounts),
							
							bb_get(succeeding_sample_sum,Old),
							sum_list_multidim(Old,List,New),
							%write((OldCounts,Counts,NewCounts,Old,List,New)),nl,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
					)
					;
					( % NOT IMPLEMENTED. old code
						query_proof(I,Query)%,W)
						->
						(
							%write(Query),nl,
							bb_get(succeeding_sample_sum,Old),
							New is Old+Weight,%*W,
							bb_put(succeeding_sample_sum,New)
						)
						;
					  	true
				  	)
			  	)
			)
		),
		fail;
		true
	),
	bb_delete(succeeding_sample_sum,SUCC),
	bb_delete(sample_sum,TOT),
	bb_delete(sample_count,CList),
	
	(
		SUCC==[] ->
		P=[]
		;
		divideby(CList,TOT,P)
	),
	divide_multidim(SUCC,CList,Avg),
%	write((SUCC,TOT,CList,Avg)),nl,
	%P is SUCC/TOT,
	!.
/*
current := current not allowed when rao is true
rao with magic=off no stratification
prior with magic=off

bayesfilter_prediction(I) :-
	(
		query_proof(I,current(S,Value,_)),
		findall(P,query_proof(I,(transition(S,Value,VOld,A,Ptr),action(A),current(S,VOld,Pold),P is Ptr*Pold)),List),
		sum_list(List,TotP),
		recorda(I,prediction(S,Value,TotP),_),
		fail;
		true
	).

bayesfilter_update(I,Temp) :-
	bb_put(bayesfilter_sum,0.0),
	(
		query_proof(I,prediction(S,Value,TotP)),
		query_proof(I,weight(observation(O),S,Value,W)),% to check
		P is TotP*W,
		recorda(Temp,next(S,Value,P),_),
		bb_get(bayesfilter_sum,Old),
		New is Old+P,
		bb_put(bayesfilter_sum,New),
		fail;
		true
	),
	%findall(A,(recorded(Temp,A,_),write(A),nl),_),
	bb_delete(bayesfilter_sum,SUM),
	eraseall(I),
	(
		query_proof(Temp,next(S,Value,P)),
		NormP is P/SUM,
		recorda(I,current(S,Value,NormP),_),
		fail;
		true
	),
	eraseall(Temp).

step_bayesfilter(Actions,PosEvidence) :-
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
	list_weight(PosEvidence,ListProb),
	init_query_list(bayes,ListProb), % add callmagic for weight
	assert_list(bayes,PosEvidence), % assert observations
	assert_list(bayes,Actions), % assert Actions
	init_query(bayes,transition(_,_,_,_,_)),
	findall(A,(recorded(bayes,A,_),write(A),nl),_),	
	nl,
	generate_sample_particlefilter(bayes,MaxP),
	findall(A,(recorded(bayes,A,_),write(A),nl),_),
	nl,
	bayesfilter_prediction(bayes),
	findall(A,(recorded(bayes,A,_),write(A),nl),_),
	nl,
	bayesfilter_update(bayes,bayes2).


init_bayesfilter2 :-
	retractall(user:evidence(_,_)),
	retractall(user:timestep(_)),
	assert(user:timestep(-1)),
	magic,
	clean_sample(bayes),
	clean_sample(bayes2),
	(
		user:init(A,B,C),
		ground(A),
		ground(B),
		ground(C),
		recorda(bayes,current(A,B,C),_),
		fail;
		true
	).
	
step_bayesfilter2(Actions,PosEvidence) :-
	get_max_priority(MaxP),
	user:timestep(Prec),
	retractall(user:timestep(_)),
	T is Prec+1,
	assert(user:timestep(T)),
%	assert_list(bayes,PosEvidence), % assert observations
	assert_list(bayes,Actions), % assert Actions
	findall(A,(recorded(bayes,A,_),write(A),nl),_),	
	nl,
	bayesfilter_prediction2(bayes,PosEvidence),
	findall(A,(recorded(bayes,A,_),write(A),nl),_),
	nl,
	bayesfilter_update2(bayes,bayes2,PosEvidence).
	
bayesfilter_prediction2(I,PosEvidence) :-
	(
		query_proof(I,current(S,Value,_)),
		findall(P,(query_proof(I,(action(A),current(S,VOld,Pold))),user:transition(S,Value,VOld,A,Ptr),P is Ptr*Pold),List),
		sum_list(List,TotP),
		recorda(I,prediction(S,Value,TotP),_),
		fail;
		true
	),
	(
		recorded(I,action(_),R),
		erase(R),
		fail;
		true
	),
	(
		recorded(I,current(_,_,_),R),
		erase(R),
		fail;
		true
	),
	(
		member(Ob,PosEvidence),
		user:weight(Ob,S1,V1,W),
		query_proof(I,\+prediction(S1,V1,_)),
		user:init(S1,V1,P1),
		recorda(I,prediction(S1,V1,P1),_),
		fail;
		true
	).

bayesfilter_update2(I,Temp,PosEvidence) :-
	(
		%query_proof(I,prediction(S,_,_)),
		member(O,PosEvidence),
		user:weight(O,S,_,_),
		bb_put(bayesfilter_sum,0.0),
		(
			recorded(I,prediction(S,Value,TotP),Ref),
			erase(Ref),
			bb_put(bayesfilter_product,1.0),
			(
				member(O,PosEvidence),
				user:weight(O,S,Value,W),
				bb_get(bayesfilter_product,OldW),
				NewW is OldW*W,
				bb_put(bayesfilter_product,NewW),
				fail;
				true
			),
			bb_delete(bayesfilter_product,Weight),
			P is TotP*Weight,
			recorda(Temp,next(S,Value,P),_),
			bb_get(bayesfilter_sum,Old),
			New is Old+P,
			bb_put(bayesfilter_sum,New),
			fail;
			true
		),
		findall(A,(recorded(Temp,A,_),write(A),nl),_),
		bb_delete(bayesfilter_sum,SUM),
		(
			query_proof(Temp,next(S,Value,P)),
			NormP is P/SUM,
			%(
			%	recorded(I,current(S,Value,_),R)
			%	->
			%		erase(R);
			%  		true
			%
			%),
			recorda(I,current(S,Value,NormP),_),
			fail;
			true
		),
		eraseall(Temp),
		fail;
		true
	),
	(
		recorded(I,prediction(SS,VV,PP),RR),
		recorda(I,current(SS,VV,PP),_),
		erase(RR),
		fail;
		true
	).
*/
