% pathfinding.pl

:- use_module(library(heaps)).
:- consult('tile_sets.pl').

% Define is_heap_empty/1 predicate
is_heap_empty(Heap) :-
    heap_size(Heap, Size),
    Size =:= 0.

% Base case: Start and Goal are the same
find_path(_, pos(X, Y), pos(X, Y), [pos(X, Y)]) :- !.

% Main find_path predicate
find_path(Map, Start, Goal, Path) :-
    heuristic(Start, Goal, H),
    get_tile(Map, Start, StartTile),
    tile_traversal_cost(StartTile, StartCost),
    G is StartCost,
    F is G + H,
    empty_heap(OpenHeap),
    add_to_heap(OpenHeap, F, [Start, G, H, [Start]], OpenSet),
    astar(OpenSet, Map, Goal, [], PathRev),
    reverse(PathRev, Path).

% Heuristic function (Manhattan distance)
heuristic(pos(X1, Y1), pos(X2, Y2), H) :-
    DX is abs(X1 - X2),
    DY is abs(Y1 - Y2),
    H is DX + DY.

% Get tile at a position (adjusted for zero-based indexing)
get_tile(Map, pos(X, Y), Tile) :-
    nth0(Y, Map, Row),
    nth0(X, Row, Tile).

% Get traversal cost of a tile
tile_traversal_cost(Tile, Cost) :-
    tile_set(Tile, Cost, _, _, _, _, _).

% Check if position is within map (adjusted for zero-based indexing)
within_map(Map, pos(X, Y)) :-
    Y >= 0,
    length(Map, NumRows),
    Y < NumRows,
    nth0(Y, Map, Row),
    X >= 0,
    length(Row, NumCols),
    X < NumCols.

% Find neighboring positions (up, down, left, right)
neighbors(Map, pos(X, Y), Neighbors) :-
    findall(pos(NX, NY),
        (   (NX is X - 1, NX >= 0, NY = Y, within_map(Map, pos(NX, NY)));
            (NX is X + 1, NY = Y, within_map(Map, pos(NX, NY)));
            (NX = X, NY is Y - 1, NY >= 0, within_map(Map, pos(NX, NY)));
            (NX = X, NY is Y + 1, within_map(Map, pos(NX, NY)))
        ),
        Neighbors).

% A* search implementation
astar(OpenSet, _, _, _, _) :-
    is_heap_empty(OpenSet),
    !,
    fail.

astar(OpenSet, Map, Goal, ClosedSet, Path) :-
    get_from_heap(OpenSet, _F, [CurrentPos, G, _H, CurrentPath], RestOpenSet),
    (   CurrentPos = Goal ->
        Path = CurrentPath
    ;   member(CurrentPos, ClosedSet) ->
        astar(RestOpenSet, Map, Goal, ClosedSet, Path)
    ;   expand_node(CurrentPos, G, CurrentPath, Map, Goal, ClosedSet, Children),
        add_children_to_heap(RestOpenSet, Children, NewOpenSet),
        astar(NewOpenSet, Map, Goal, [CurrentPos | ClosedSet], Path)
    ).

% Expand current node
expand_node(CurrentPos, G, CurrentPath, Map, Goal, ClosedSet, Children) :-
    neighbors(Map, CurrentPos, NeighborPositions),
    findall([NeighborPos, GNew, H, [NeighborPos | CurrentPath]],
        (
            member(NeighborPos, NeighborPositions),
            \+ member(NeighborPos, ClosedSet),
            get_tile(Map, NeighborPos, Tile),
            tile_traversal_cost(Tile, Cost),
            Cost > 0, % Ensure the tile is traversable
            GNew is G + Cost,
            heuristic(NeighborPos, Goal, H)
        ),
        Children).

% Add children to heap
add_children_to_heap(OpenSet, [], OpenSet).
add_children_to_heap(OpenSet, [[Pos, G, H, Path] | Rest], NewOpenSet) :-
    F is G + H,
    add_to_heap(OpenSet, F, [Pos, G, H, Path], TempOpenSet),
    add_children_to_heap(TempOpenSet, Rest, NewOpenSet).

% Calculate path cost
find_path_cost(Path, Map, Cost) :-
    calculate_cost(Path, Map, 0, Cost).

calculate_cost([_], _, AccCost, AccCost).
calculate_cost([pos(X1, Y1), pos(X2, Y2) | Rest], Map, AccCost, Cost) :-
    get_tile(Map, pos(X2, Y2), Tile),
    tile_traversal_cost(Tile, TileCost),
    NewAccCost is AccCost + TileCost,
    calculate_cost([pos(X2, Y2) | Rest], Map, NewAccCost, Cost).