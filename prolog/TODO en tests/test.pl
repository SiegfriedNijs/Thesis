:- use_module('../distributionalclause.pl').
:- use_module('../dcpf.pl').
:- use_module('../random/sampling.pl').

:- init. %%%%NODIG!!!
:- set_magic(backward).
:- set_debug(false).

has_performer(p1)       ~= finite([0.9:pr2robot1,0.1:human1]) := true.
has_category(pr2robot1) ~= finite([1:pr2_ca])    := true.
has_category(human1) ~ finite([1:humans_ca]) := true.
