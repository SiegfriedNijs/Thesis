%%% -*- Mode: Prolog; -*-

:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').
:- use_module(library(lists)).

builtin(true).
builtin(findall(_,_,_)).
builtin(length(_,_)).
builtin(member(_,_)).
builtin(timestep(_)).
builtin(A=B).
builtin(A\==B).
builtin(A\=B).
builtin(A is B).
builtin(A > B).
builtin(A < B).
builtin(A >= B).
builtin(A =< B).
builtin(between(_,_,_)).
builtin(min_list(_,_)).
builtin(max_list(_,_)).
builtin(sum_list(_,_)).
builtin(min(_,_)).
builtin(nth1(_,_,_)).
builtin(varia(_,_,_)).
builtin(product(_,_)).
builtin(densityGaussian(_,_,_,_)).
builtin(checkvalue(_,_)).

satisfiable(A=B).
satisfiable(A\==B).
satisfiable(A\=B).
satisfiable(A > B).
satisfiable(A < B).
satisfiable(A >= B).
satisfiable(A =< B).
satisfiable(findall(_,_,_)).

checkvalue(true,1.0).
checkvalue(false,0.0).

builtin(\+A) :-
	builtin(A).
builtin(write(_)).

:- set_magic(backward).

lifted(true).
raoblackwellisation(false).
%rao(position).
%rao(gold(X,Y)).


maze(X,Y):0 ~ finite([0.0:pit,0.6:free,0.4:wall])  := (X,Y)\=(0,0).

maze(0,0):0 ~ uniform([free]) := true.

%wumpus:t ~ uniform([(-3,-3),(-3,-2),(-3,-1),(-3,0),(-3,1),(-3,2),(-3,3),(-2,-3),(-2,-2),(-2,-1),(-2,0),(-2,1),(-2,2),(-2,3),(-1,-3),(-1,-2),(-1,-1),(-1,0),(-1,1),(-1,2),(-1,3),(0,-3),(0,-2),(0,-1),(0,0),(0,1),(0,2),(0,3),(1,-3),(1,-2),(1,-1),(1,0),(1,1),(1,2),(1,3),(2,-3),(2,-2),(2,-1),(2,0),(2,1),(2,2),(2,3),(3,-3),(3,-2),(3,-1),(3,0),(3,1),(3,2),(3,3)]) := true.%findall((X,Y),(between(-3,3,X),between(-3,3,Y)),D).

gold(X,Y):0 ~ finite([0.1:gold,0.9:null]) := true.

position:0 ~ finite([1:(0,0)]) := true.

%energy:0 ~ finite([1:1]) := true.


%energy:t+1 ~ gaussian(NewVal,0.0001) :=
%	energy:t ~= Val,
%	NewVal is Val*0.99.
gold(X,Y):t+1 ~ finite([1:Val]) := 
	gold(X,Y):t ~= Val.
	
maze(X,Y):t+1 ~ finite([1:Val]) := 
	maze(X,Y):t ~= Val.%,X=<2.
	
%maze(X,Y):t+1 ~ finite([1:Val]) := 
%	maze(X,Y):t ~= Val,X>2.
/*
maze(X,Y):t+1 ~ finite([0.999:free,0.001:wall]) := 
	maze(X,Y):t ~= free.
maze(X,Y):t+1 ~ finite([0.999:wall,0.001:free]) := 
	maze(X,Y):t ~= wall.
*/	
/*
change:t ~ finite([0.99:false,0.01:true]) := true.
wumpus:t+1 ~ finite([1:Val]) :=
	change:t ~= false,
	wumpus:t ~= Val.
	
wumpus:t+1 ~ uniform(D) := 
	change:t ~= true,
	findall((X,Y),(between(-2,2,X),between(-2,2,Y)),D).	
*/

wumpus:t+1 ~ finite([1:Val]) := 
	wumpus:t ~= Val.
	
/*
gold(X,Y):t+1 ~ finite([1:Val]) := 
	gold(X,Y):t ~= Val.

gold(X,Y):t+1 ~ finite([1:gold,0.000:null]) := 
	gold(X,Y):t ~= gold.
gold(X,Y):t+1 ~ finite([1:null,0.000:gold]) := 
	gold(X,Y):t ~= null.
*/
position:t+1 ~ finite([0.1:(Xold,Yold),0.9:(X,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(left),
	X is Xold - 1,
	\+(maze(X,Yold):t ~= wall).
	
position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(left),
	X is Xold - 1,
	maze(X,Yold):t ~= wall.

position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(null).

position:t+1 ~ finite([0.1:(Xold,Yold),0.9:(X,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(right),
	X is Xold + 1,
	\+(maze(X,Yold):t ~= wall).
	
position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(right),
	X is Xold + 1,
	maze(X,Yold):t ~= wall.


position:t+1 ~ finite([0.1:(Xold,Yold),0.9:(Xold,Y)]) :=
	position:t ~= (Xold,Yold),
	action(up),
	Y is Yold + 1,
	\+(maze(Xold,Y):t ~= wall).
	
position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(up),
	Y is Yold + 1,
	maze(Xold,Y):t ~= wall.

position:t+1 ~ finite([0.1:(Xold,Yold),0.9:(Xold,Y)]) :=
	position:t ~= (Xold,Yold),
	action(down),
	Y is Yold - 1,
	\+(maze(Xold,Y):t ~= wall).
	
position:t+1 ~ finite([1:(Xold,Yold)]) :=
	position:t ~= (Xold,Yold),
	action(down),
	Y is Yold - 1,
	maze(Xold,Y):t ~= wall.
	
observation(up):t+1 ~ finite([0.9:free,0.1:wall]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y + 1,
	maze(X,Ynext):t+1 ~= free.

observation(up):t+1 ~ finite([0.9:wall,0.1:free]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y + 1,
	maze(X,Ynext):t+1 ~= wall.

observation(down):t+1 ~ finite([0.9:free,0.1:wall]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y - 1,
	maze(X,Ynext):t+1 ~= free.

observation(down):t+1 ~ finite([0.9:wall,0.1:free]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y - 1,
	maze(X,Ynext):t+1 ~= wall.
	
observation(left):t+1 ~ finite([0.9:free,0.1:wall]) :=
	position:t+1 ~= (X,Y),
	Xnext is X - 1,
	maze(Xnext,Y):t+1 ~= free.

observation(left):t+1 ~ finite([0.9:wall,0.1:free]) :=
	position:t+1 ~= (X,Y),
	Xnext is X - 1,
	maze(Xnext,Y):t+1 ~= wall.
	
observation(right):t+1 ~ finite([0.9:free,0.1:wall]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	maze(Xnext,Y):t+1 ~= free.

observation(right):t+1 ~ finite([0.9:wall,0.1:free]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	maze(Xnext,Y):t+1 ~= wall.


observation(stench):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	wumpus:t+1 ~= (Xnext,Y).
	
observation(stench):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X - 1,
	wumpus:t+1 ~= (Xnext,Y).

observation(stench):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y + 1,
	wumpus:t+1 ~= (X,Ynext).

observation(stench):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y - 1,
	wumpus:t+1 ~= (X,Ynext).

observation(stench):t+1 ~ finite([0.1:true,0.9:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	Ynext is Y + 1,
	Xprec is X - 1,
	Yprec is Y - 1,
	\+(wumpus:t+1 ~= (Xnext,Y)),
	\+(wumpus:t+1 ~= (X,Ynext)),
	\+(wumpus:t+1 ~= (Xprec,Y)),
	\+(wumpus:t+1 ~= (X,Yprec)).
	
observation(breeze):t+1 ~ finite([1.0:true,0.0:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	maze(Xnext,Y):t+1 ~= pit.
	
observation(breeze):t+1 ~ finite([1.0:true,0.0:false]) :=
	position:t+1 ~= (X,Y),
	Xnext is X - 1,
	maze(Xnext,Y):t+1 ~= pit.

observation(breeze):t+1 ~ finite([1.0:true,0.0:false]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y + 1,
	maze(X,Ynext):t+1 ~= pit.

observation(breeze):t+1 ~ finite([1.0:true,0.0:false]) :=
	position:t+1 ~= (X,Y),
	Ynext is Y - 1,
	maze(X,Ynext):t+1 ~= pit.

observation(breeze):t+1 ~ finite([1.0:false,0.0:true]) :=
	position:t+1 ~= (X,Y),
	Xnext is X + 1,
	Ynext is Y + 1,
	Xprec is X - 1,
	Yprec is Y - 1,
	\+(maze(Xnext,Y):t+1 ~= pit),
	\+(maze(X,Ynext):t+1 ~= pit),
	\+(maze(Xprec,Y):t+1 ~= pit),
	\+(maze(X,Yprec):t+1 ~= pit).


observation(gold):t+1 ~ finite([0.2:true,0.8:false]) :=
	position:t+1 ~= (X,Y),
	gold(X,Y):t+1 ~= null.

	
observation(gold):t+1 ~ finite([0.9:true,0.1:false]) :=
	position:t+1 ~= (X,Y),
	gold(X,Y):t+1 ~= gold.


start :-

init_particle(100),
writeln('11111'),
printp(1),
step_particle([action(null)],[observation(up) ~= wall,observation(right) ~= free],100,1),
writeln('22222'),
printp(1),
writeln(R2e).



mazesizemin(-4).
mazesizemax(4).

plotdata(Pos) :-
	bb_get(dcpf:offset,Offset),
	I is Offset+Pos,
	mazesizemin(Min),
	mazesizemax(Max),
	Tot is Max-Min,
	(
		between(0,Tot,YY),
		(
			between(Min,Max,X),
			(
				Y is Max-YY,
				(
					distributionalclause:proof_query_backward(I,current(wumpus) ~= (X,Y)) ->
						write('WW')
					;
					(
						recorded(I,current(maze(X,Y)) ~= _,_)
						->
						(
								distributionalclause:proof_query_backward(I,current(maze(X,Y)) ~= V),
								V==free
								->
								(
									distributionalclause:proof_query_backward(I,current(position) ~= (X,Y)) ->
									(
										distributionalclause:proof_query_backward(I,current(gold(X,Y)) ~= gold) ->
										write('A$')
										;
										write('AA')
									)
									;
									(
										distributionalclause:proof_query_backward(I,current(gold(X,Y)) ~= gold) ->
										write('$$')
										;
										write('  ')
									)
								)
								;
								write('##')
						)
						;
						write('??')
					)
				)
			),
			fail;
			true
		),
		nl,
		fail;
		true
	).

		
averageobject(Particles,Mean) :-
	dcpf:bb_get(offset,Offset),
	bb_put(sumobj,0.0),
	(
		between(1,Particles,Pos),
		I is Offset+Pos,
		recorded(I,current(object) ~= Val,_),
		bb_get(sumobj,OldTOT),
		NewTOT is OldTOT+Val,
		bb_put(sumobj,NewTOT),
		fail;
		true
	),
	bb_delete(sumobj,T),
	Mean is T/Particles.
	
