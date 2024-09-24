% Define the rules for the map generation

% Forest must be surrounded by grass
surrounded_by_grass(X, Y, Map) :-
	adjacent(X, Y, Map, grass).

% Water becomes ocean if it exceeds a certain amount and must be surrounded by sand
becomes_ocean(X, Y, Map) :-
	water_percentage(Map, P),
	P > 50, % Example threshold
	adjacent(X, Y, Map, sand).

% Forest cannot be immediately next to sand
not_next_to_sand(X, Y, Map) :-
	\+ adjacent(X, Y, Map, sand).

% Helper predicate to check adjacent cells
adjacent(X, Y, Map, Type) :-
	NX is X + 1, NY is Y, get_cell(NX, NY, Map, Type);
	NX is X - 1, NY is Y, get_cell(NX, NY, Map, Type);
	NX is X, NY is Y + 1, get_cell(NX, NY, Map, Type);
	NX is X, NY is Y - 1, get_cell(NX, NY, Map, Type).

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