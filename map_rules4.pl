% Unfortunately, procedural generation did not work out as planned, so I pivioted to
% Use noise generation to create the map. This is the code that was used to generate the map
% Next, 'correction' procedure will be performed on the map to make it align with the constraints.

:- use_module(library(random)).
:- consult('tile_sets.pl').

% Generate map predicate
generate_map(Width, Height, Percentages, Map) :-
    create_empty_map(Width, Height, EmptyMap),
    calculate_cumulative_percentages(Percentages, CumulativePercentages),
    fill_map(EmptyMap, CumulativePercentages, Map).

% Fill the map with tiles based on cumulative percentages
fill_map([], _, []).
fill_map([Row|Rows], CumulativePercentages, [FilledRow|FilledRows]) :-
    fill_row(Row, CumulativePercentages, FilledRow),
    fill_map(Rows, CumulativePercentages, FilledRows).

fill_row([], _, []).
fill_row([_|Cells], CumulativePercentages, [Tile|FilledCells]) :-
    random(0.0, 100.0, Rand),
    select_tile(CumulativePercentages, Rand, Tile),
    fill_row(Cells, CumulativePercentages, FilledCells).

% Fill the map with tiles based on percentages and add noise
fill_map(Map, Percentages, Width, Height, FilledMap) :-
	TotalPositions is Width * Height,
	calculate_tile_counts(Percentages, TotalPositions, TileCounts),
	generate_tiles(TileCounts, Tiles),
	findall((X, Y), (between(1, Width, X), between(1, Height, Y)), AllPositions),
	add_noise_to_positions(AllPositions, Width, Height, NoisyPositions),
	random_permutation(NoisyPositions, ShuffledPositions),
	assign_tiles(Map, ShuffledPositions, Tiles, FilledMap).

% Select a tile based on cumulative percentages
select_tile([Tile-Cumulative|_], Rand, Tile) :-
    Rand =< Cumulative, !.
select_tile([_|Rest], Rand, Tile) :-
    select_tile(Rest, Rand, Tile).


% Add noise to positions
add_noise_to_positions(Positions, Width, Height, NoisyPositions) :-
	maplist(add_noise(Width, Height), Positions, NoisyPositions).

add_noise(Width, Height, (X, Y), (X1, Y1)) :-
	random_between(-1, 1, NoiseX),
	random_between(-1, 1, NoiseY),
	X1 is max(1, min(Width, X + NoiseX)),
	Y1 is max(1, min(Height, Y + NoiseY)).

% Create an empty map
create_empty_map(Width, Height, Map) :-
	length(Map, Height),
	maplist(length_(Width), Map).

length_(Length, List) :- length(List, Length).

% Calculate cumulative percentages
calculate_cumulative_percentages(Percentages, CumulativePercentages) :-
    percentages_to_cumulative(Percentages, 0, CumulativePercentages).

percentages_to_cumulative([], _, []).
percentages_to_cumulative([Tile-Percentage|Rest], Acc, [Tile-Cumulative|CumulativeRest]) :-
    Cumulative is Acc + Percentage,
    percentages_to_cumulative(Rest, Cumulative, CumulativeRest).

% Calculate the number of tiles for each type based on percentages
calculate_tile_counts(Percentages, TotalPositions, TileCounts) :-
	findall(Tile-Count, (
		member(Tile-Percentage, Percentages),
		Count is round(Percentage / 100 * TotalPositions),
		Count > 0
	), TileCounts).

% Generate a list of tiles based on the counts
generate_tiles([], []).
generate_tiles([Tile-Count|Rest], Tiles) :-
	length(TileList, Count),
	maplist(=(Tile), TileList),
	generate_tiles(Rest, RestTiles),
	append(TileList, RestTiles, Tiles).

% Assign tiles to positions
assign_tiles(Map, _, [], Map).
assign_tiles(Map, [(X, Y)|Positions], [Tile|Tiles], FilledMap) :-
	place_tile(Map, X, Y, Tile, TempMap),
	assign_tiles(TempMap, Positions, Tiles, FilledMap).

% Place a single tile on the map
place_tile(Map, X, Y, Tile, FilledMap) :-
	nth1(Y, Map, Row),
	replace_nth1(X, Row, Tile, NewRow),
	replace_nth1(Y, Map, NewRow, FilledMap).

% Replace the Nth element in a list
replace_nth1(1, [_|Rest], Elem, [Elem|Rest]).
replace_nth1(N, [Head|Tail], Elem, [Head|NewTail]) :-
	N > 1,
	N1 is N - 1,
	replace_nth1(N1, Tail, Elem, NewTail).
