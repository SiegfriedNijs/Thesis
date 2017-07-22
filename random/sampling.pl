%%% -*- Mode: Prolog; -*-


%:- style_check(all).
:- yap_flag(unknown,error).

:- module(sampling,[sample_uniform/3,draw_uniform/2,matrixproduct/4,optimalproposal/7,poisson/2,student/2,studentPdf/3,gamma/3,normal/3,normal2/6,densityGaussian/4,dirichlet/2,gaussian/3,kalman/13,kalmanrao/14,kalmanrao_simplified/9]).

:- load_foreign_files(['sampling'],[],init_my_predicates).

:- use_module(library(lists)).

draw_uniform([H|T],Element) :-
	length([H|T],Len),
	I is integer(random*Len),
	nth0(I,[H|T],Element).

sample_uniform(A,B,X) :-
	number(A),
	number(B),
	A =< B,
	X is random*(B-A)+A.