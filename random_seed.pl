:- use_module(library(random)).

random_seed(Seed) :-
	get_time(Time),
	Seed is round(Time) mod 10000.