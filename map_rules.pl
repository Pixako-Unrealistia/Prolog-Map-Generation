:- use_module(library(random)).
:- use_module(library(lists)).

% Define the rules for the map generation

% Forest must be surrounded by grass
surrounded_by_grass(X, Y, Map) :-
    adjacent(X, Y, Map, grass).

% Water becomes ocean if it exceeds a certain amount and must be surrounded by sand
becomes_ocean(X, Y, Map) :-
    water_percentage(Map, P),
    P > 30, % Example threshold
    adjacent(X, Y, Map, sand).

% Forest cannot be immediately next to sand
not_next_to_sand(X, Y, Map) :-
    \+ adjacent(X, Y, Map, sand).

% Helper predicate to check adjacent cells
adjacent(X, Y, Map, Type) :-
    NX1 is X + 1, NY1 is Y, get_cell(NX1, NY1, Map, Type);
    NX2 is X - 1, NY2 is Y, get_cell(NX2, NY2, Map, Type);
    NX3 is X, NY3 is Y + 1, get_cell(NX3, NY3, Map, Type);
    NX4 is X, NY4 is Y - 1, get_cell(NX4, NY4, Map, Type).

% Helper predicate to get the type of a cell
get_cell(X, Y, Map, Type) :-
    nth0(Y, Map, Row),
    nth0(X, Row, Type).

% Calculate water percentage
water_percentage(Map, P) :-
    flatten(Map, FlatMap),
    include(==(water), FlatMap, WaterCells),
    length(WaterCells, WaterCount),
    length(FlatMap, TotalCount),
    P is (WaterCount / TotalCount) * 100.

% Generate the map based on the rules
generate_map(Width, Height, WaterPercentage, ForestPercentage, LandPercentage, Map) :-
    length(Map, Height),
    maplist(length_(Width), Map),
    initialize_map(Map),
    generate_noise_map(Width, Height, NoiseMap),
    classify_terrain(NoiseMap, WaterPercentage, ForestPercentage, LandPercentage, ClassifiedMap),
    enforce_rules(ClassifiedMap, FinalMap),
    Map = FinalMap,
    !.

length_(Length, List) :- length(List, Length).

initialize_map([]).
initialize_map([Row|Rows]) :-
    maplist(=(empty), Row),
    initialize_map(Rows).

% Generate a noise map for terrain generation
generate_noise_map(Width, Height, NoiseMap) :-
    length(NoiseMap, Height),
    maplist(generate_noise_row(Width), NoiseMap).

generate_noise_row(Width, Row) :-
    length(Row, Width),
    maplist(generate_noise_value, Row).

generate_noise_value(Value) :-
    random_between(0, 100, Value).

% Classify terrain based on noise values
classify_terrain(Map, WaterPercentage, ForestPercentage, LandPercentage, ClassifiedMap) :-
    flatten(Map, FlatMap),
    length(FlatMap, _TotalCells), % Fixed singleton variable
    WaterThreshold is WaterPercentage,
    ForestThreshold is WaterPercentage + ForestPercentage,
    LandThreshold is ForestThreshold + LandPercentage,
    maplist(classify_cell(WaterThreshold, ForestThreshold, LandThreshold), FlatMap, ClassifiedFlatMap),
    reshape_map(ClassifiedFlatMap, Map, ClassifiedMap).

classify_cell(WaterThreshold, ForestThreshold, LandThreshold, Value, Type) :-
    (Value =< WaterThreshold -> Type = water;
     Value =< ForestThreshold -> Type = forest;
     Value =< LandThreshold -> Type = land).

reshape_map([], [], []).
reshape_map(FlatMap, [Row|Rows], [ClassifiedRow|ClassifiedRows]) :-
    length(Row, Length),
    append(ClassifiedRow, RestFlatMap, FlatMap),
    length(ClassifiedRow, Length),
    reshape_map(RestFlatMap, Rows, ClassifiedRows).

% Enforce the rules on the generated map
enforce_rules(Map, FinalMap) :-
    % Ensure forests are surrounded by grass
    maplist(enforce_row_rules, Map, FinalMap).

enforce_row_rules(Row, FinalRow) :-
    maplist(enforce_cell_rules(Row), Row, FinalRow).

enforce_cell_rules(Row, Cell, FinalCell) :-
    (Cell = forest -> (surrounded_by_grass(_, _, Row) -> FinalCell = forest; FinalCell = grass);
     Cell = water -> (becomes_ocean(_, _, Row) -> FinalCell = ocean; FinalCell = water);
     FinalCell = Cell).