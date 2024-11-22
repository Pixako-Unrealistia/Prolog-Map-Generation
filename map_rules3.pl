% This is 3rd rendition of generation algorithm, emphasizing on making everything looks nice
% Use noise generation to create the map. This is the code that was used to generate the map
% Next, 'correction' procedure will be performed on the map to make it align with the constraints.

:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('tile_sets.pl').

% Generate map predicate.
generate_map(Width, Height, Percentages, Map) :-
	set_random_seed(GlobalSeed),
	calculate_cumulative_percentages(Percentages, CumulativePercentages),
	generate_noise_map(GlobalSeed, Width, Height, NoiseMap),
	fill_map_with_noise(NoiseMap, CumulativePercentages, Map).

% Generate a noise map based on implementation below.
generate_noise_map(GlobalSeed, Width, Height, NoiseMap) :-
	MaxX is Width - 1,
	MaxY is Height - 1,
	findall(Row, (
		between(0, MaxY, Y),
		findall(NoiseValue, (
			between(0, MaxX, X),
			perlin_noise(GlobalSeed, X / Width, Y / Height, NoiseValue)
		), Row)
	), NoiseMap).

% As prolog does not appear to have perlin_noise built-in, this is my attempt at replication.
perlin_noise(GlobalSeed, X, Y, NoiseValue) :-
	Xi is floor(X * 10),
	Yi is floor(Y * 10),
	Xf is (X * 10) - Xi,
	Yf is (Y * 10) - Yi,
	random_hash(GlobalSeed, Xi, Yi, A),
	random_hash(GlobalSeed, Xi + 1, Yi, B),
	random_hash(GlobalSeed, Xi, Yi + 1, C),
	random_hash(GlobalSeed, Xi + 1, Yi + 1, D),
	fade(Xf, U),
	fade(Yf, V),
	lerp(U, A, B, AB),
	lerp(U, C, D, CD),
	lerp(V, AB, CD, NoiseValueUnscaled),
	NoiseValue is (NoiseValueUnscaled + 1) * 50.

% Fade function for smoothing
fade(T, Faded) :-
	Faded is T * T * T * (T * (T * 6 - 15) + 10).

% Linear interpolation to smooth the noise.
lerp(T, A0, A1, Result) :-
	Result is A0 + T * (A1 - A0).

% Hash function to generate pseudo-random gradients..
random_hash(GlobalSeed, X, Y, Result) :-
	Seed is GlobalSeed + X * 49632 + Y * 325176 + 1,
	set_random(seed(Seed)),
	random(R),
	Result is R * 2 - 1.

% Calculate cumulative percentages.
calculate_cumulative_percentages(Percentages, CumulativePercentages) :-
	percentages_to_cumulative(Percentages, 0, CumulativePercentages).

percentages_to_cumulative([], _, []).
percentages_to_cumulative([Tile-Percentage|Rest], Acc, [Tile-Cumulative|CumulativeRest]) :-
	Cumulative is Acc + Percentage,
	percentages_to_cumulative(Rest, Cumulative, CumulativeRest).

% Fill the map based on noise values.
fill_map_with_noise([], _, []).
fill_map_with_noise([NoiseRow|NoiseRows], CumulativePercentages, [FilledRow|FilledRows]) :-
	fill_row_with_noise(NoiseRow, CumulativePercentages, FilledRow),
	fill_map_with_noise(NoiseRows, CumulativePercentages, FilledRows).

fill_row_with_noise([], _, []).
fill_row_with_noise([Noise|Noises], CumulativePercentages, [Tile|FilledCells]) :-
	select_tile(CumulativePercentages, Noise, Tile),
	fill_row_with_noise(Noises, CumulativePercentages, FilledCells).

% Select a tile based on cumulative percentages calculated above.
select_tile([Tile-Cumulative|_], Value, Tile) :-
	Value =< Cumulative, !.
select_tile([_|Rest], Value, Tile) :-
	select_tile(Rest, Value, Tile).

set_random_seed(GlobalSeed) :-
	get_time(Time),
	GlobalSeed is floor(Time * 1000).
