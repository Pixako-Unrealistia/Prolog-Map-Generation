% Current algo, generate -> shuffle -> assign
% To do, implement selection rules (be next to, can not be next to.)

:- consult('tile_sets.pl').

% Generate map predicate
legacy_generate_map(Width, Height, Percentages, Map) :-
	legacy_create_empty_map(Width, Height, EmptyMap),
	legacy_fill_map(EmptyMap, Percentages, Width, Height, Map).

% Create an empty map
legacy_create_empty_map(Width, Height, Map) :-
	length(Map, Height),
	maplist(length_(Width), Map).

length_(Length, List) :- length(List, Length).

% Fill the map with tiles based on percentages
legacy_fill_map(Map, Percentages, Width, Height, FilledMap) :-
	TotalPositions is Width * Height,
	legacy_calculate_tile_counts(Percentages, TotalPositions, TileCounts),
	legacy_generate_tiles(TileCounts, Tiles),
	findall((X, Y), (between(1, Width, X), between(1, Height, Y)), AllPositions),
	random_permutation(AllPositions, ShuffledPositions),
	legacy_assign_tiles(Map, ShuffledPositions, Tiles, FilledMap).

% Calculate the number of tiles for each type based on percentages
legacy_calculate_tile_counts(Percentages, TotalPositions, TileCounts) :-
	findall(Tile-Count, (
		member(Tile-Percentage, Percentages),
		Count is round(Percentage / 100 * TotalPositions),
		Count > 0
	), TileCounts).

% Generate a list of tiles based on the counts
legacy_generate_tiles([], []).
legacy_generate_tiles([Tile-Count|Rest], Tiles) :-
	length(TileList, Count),
	maplist(=(Tile), TileList),
	legacy_generate_tiles(Rest, RestTiles),
	append(TileList, RestTiles, Tiles).

% Assign tiles to positions
legacy_assign_tiles(Map, _, [], Map).
legacy_assign_tiles(Map, [(X, Y)|Positions], [Tile|Tiles], FilledMap) :-
	legacy_assign_tiles(Map, X, Y, Tile, TempMap),
	legacy_assign_tiles(TempMap, Positions, Tiles, FilledMap).

% Place a single tile on the map
legacy_assign_tiles(Map, X, Y, Tile, FilledMap) :-
	nth1(Y, Map, Row),
	legacy_replace_nth1(X, Row, Tile, NewRow),
	legacy_replace_nth1(Y, Map, NewRow, FilledMap).

% Replace the Nth element in a list
legacy_replace_nth1(1, [_|Rest], Elem, [Elem|Rest]).
legacy_replace_nth1(N, [Head|Tail], Elem, [Head|NewTail]) :-
	N > 1,
	N1 is N - 1,
	legacy_replace_nth1(N1, Tail, Elem, NewTail).
