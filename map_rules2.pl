% Load tile sets from the external file
:- consult('tile_sets.pl').

% Define tile set facts
tile_set(water, 3, [], [], 'blue', true, '').
tile_set(deep_water, 5, [sand, forest, land], [water], 'darkblue', false, '').
tile_set(forest, 2, [], [], 'green', true, '').
tile_set(land, 1, [], [], 'yellow', true, '').
tile_set(sand, 1, [], [], 'lightyellow', false, '').

% Generate map predicate
generate_map(Width, Height, Percentages, Map) :-
    create_empty_map(Width, Height, EmptyMap),
    fill_map(EmptyMap, Percentages, Width, Height, Map).

% Create an empty map
create_empty_map(Width, Height, Map) :-
    length(Map, Height),
    maplist(length_(Width), Map).

length_(Length, List) :- length(List, Length).

% Fill the map with tiles based on percentages
fill_map(Map, Percentages, Width, Height, FilledMap) :-
    findall(Tile, (member(Tile-Percentage, Percentages), between(1, Percentage, _)), Tiles),
    random_permutation(Tiles, ShuffledTiles),
    place_tiles(Map, ShuffledTiles, Width, Height, FilledMap).

% Place tiles on the map
place_tiles(Map, [], _, _, Map).
place_tiles(Map, [Tile|Tiles], Width, Height, FilledMap) :-
    random_between(1, Width, X),
    random_between(1, Height, Y),
    place_tile(Map, X, Y, Tile, TempMap),
    place_tiles(TempMap, Tiles, Width, Height, FilledMap).

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
