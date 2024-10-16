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

% Ensure the specified number of ocean bodies
ensure_ocean_count(Map, OceanCount) :-
    findall((X, Y), get_cell(X, Y, Map, water), WaterCells),
    length(WaterCells, WaterCount),
    WaterCount >= OceanCount.

% Ensure the specified number of forest bodies
ensure_forest_count(Map, ForestCount) :-
    findall((X, Y), get_cell(X, Y, Map, forest), ForestCells),
    length(ForestCells, ForestCount).

% Generate the map based on the rules
generate_map(Width, Height, WaterPercentage, ForestPercentage, LandPercentage, OceanCount, ForestCount, Map) :-
    length(Map, Height),
    maplist(length_(Width), Map),
    initialize_map(Map),
    place_oceans(Map, OceanCount),
    place_forests(Map, ForestCount),
    fill_remaining(Map, WaterPercentage, ForestPercentage, LandPercentage),
    enforce_rules(Map, OceanCount, ForestCount),
    print_map(Map). % Debugging statement to print the generated map

length_(Length, List) :- length(List, Length).

initialize_map([]).
initialize_map([Row|Rows]) :-
    maplist(=(empty), Row),
    initialize_map(Rows).

place_oceans(Map, 0).
place_oceans(Map, OceanCount) :-
    OceanCount > 0,
    random_between(0, 9, X),
    random_between(0, 9, Y),
    get_cell(X, Y, Map, empty),
    set_cell(X, Y, Map, water),
    NewOceanCount is OceanCount - 1,
    place_oceans(Map, NewOceanCount).

place_forests(Map, 0).
place_forests(Map, ForestCount) :-
    ForestCount > 0,
    random_between(0, 9, X),
    random_between(0, 9, Y),
    get_cell(X, Y, Map, empty),
    set_cell(X, Y, Map, forest),
    NewForestCount is ForestCount - 1,
    place_forests(Map, NewForestCount).

fill_remaining(Map, WaterPercentage, ForestPercentage, LandPercentage) :-
    flatten(Map, FlatMap),
    include(==(empty), FlatMap, EmptyCells),
    length(EmptyCells, EmptyCount),
    WaterCells is (WaterPercentage * EmptyCount) // 100,
    ForestCells is (ForestPercentage * EmptyCount) // 100,
    LandCells is EmptyCount - WaterCells - ForestCells,
    place_cells(Map, water, WaterCells),
    place_cells(Map, forest, ForestCells),
    place_cells(Map, land, LandCells).

place_cells(_, _, 0).
place_cells(Map, Type, Count) :-
    random_between(0, 9, X),
    random_between(0, 9, Y),
    get_cell(X, Y, Map, empty),
    set_cell(X, Y, Map, Type),
    NewCount is Count - 1,
    place_cells(Map, Type, NewCount).

set_cell(X, Y, Map, Type) :-
    nth0(Y, Map, Row),
    nth0(X, Row, _, Rest),
    nth0(X, NewRow, Type, Rest),
    nth0(Y, Map, NewRow, Rest).

% Enforce the rules on the generated map
enforce_rules(Map, OceanCount, ForestCount) :-
    % Ensure forests are surrounded by grass
    forall((nth0(Y, Map, Row), nth0(X, Row, forest)), surrounded_by_grass(X, Y, Map)),
    % Ensure forests are not next to sand
    forall((nth0(Y, Map, Row), nth0(X, Row, forest)), not_next_to_sand(X, Y, Map)),
    % Ensure the specified number of ocean bodies
    ensure_ocean_count(Map, OceanCount),
    % Ensure the specified number of forest bodies
    ensure_forest_count(Map, ForestCount).

% Debugging predicates
print_map([]).
print_map([Row|Rows]) :-
    writeln(Row),
    print_map(Rows).