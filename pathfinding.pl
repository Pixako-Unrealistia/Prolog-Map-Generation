% Pathfinding module for map generator

:- module(pathfinding, [find_path/4, find_path_cost/3]).
:- use_module(library(heaps)).
:- use_module(library(lists)).
:- consult('tile_sets.pl').

% Manhattan distance heuristic
heuristic((X1,Y1), (X2,Y2), H) :-
    DX is abs(X2-X1),
    DY is abs(Y2-Y1),
    H is DX + DY.

% Get traversal cost for a tile type
% Uses the tile_set/7 facts from tile_sets.pl
get_tile_cost(Tile, Cost) :-
    tile_set(Tile, Cost, _, _, _, _, _).

% Get tile at specific position in map
get_map_tile(Map, (X,Y), Tile) :-
    nth0(Y, Map, Row),
    nth0(X, Row, Tile).

% Check if position is within map bounds
valid_pos(Map, (X,Y)) :-
    length(Map, Height),
    nth0(0, Map, FirstRow),
    length(FirstRow, Width),
    X >= 0, X < Width,
    Y >= 0, Y < Height.

% Get valid neighbors
get_neighbors(Map, (X,Y), Neighbors) :-
    Directions = [(0,1), (0,-1), (1,0), (-1,0)],
    findall(
        (NX,NY),
        (
            member((DX,DY), Directions),
            NX is X + DX,
            NY is Y + DY,
            valid_pos(Map, (NX,NY)),
            get_map_tile(Map, (NX,NY), Tile),
            get_tile_cost(Tile, _)  % Ensure tile is traversable
        ),
        Neighbors
    ).

% Reconstruct path from came_from dictionary
reconstruct_path(CameFrom, Current, [Current|Path]) :-
    get_dict(Current, CameFrom, Parent),
    reconstruct_path(CameFrom, Parent, Path), !.
reconstruct_path(_, Current, [Current]).

% Main pathfinding predicate
find_path(Map, Start, Goal, Path) :-
    empty_heap(OpenHeap),
    heuristic(Start, Goal, H),
    add_to_heap(OpenHeap, H, (Start,0,[Start]), InitialOpenHeap),
    
    % Create empty closed set
    empty_assoc(ClosedSet),
    
    % Search for path
    astar_search(Map, Goal, InitialOpenHeap, ClosedSet, Path).

% A* search implementation
astar_search(_, Goal, OpenHeap, _, Path) :-
    get_from_heap(OpenHeap, _, (Goal,_,PathToGoal), _),
    reverse(PathToGoal, Path).

astar_search(Map, Goal, OpenHeap, ClosedSet, Path) :-
    get_from_heap(OpenHeap, _, (Current,Cost,PathSoFar), RestHeap),
    \+ get_assoc(Current, ClosedSet, _),
    
    % Get neighbors
    get_neighbors(Map, Current, Neighbors),
    
    % Process each neighbor
    process_neighbors(Map, Goal, Neighbors, Current, Cost, PathSoFar, RestHeap, ClosedSet, NewOpenHeap),
    
    % Add current to closed set
    put_assoc(Current, ClosedSet, true, NewClosedSet),
    
    % Continue search
    astar_search(Map, Goal, NewOpenHeap, NewClosedSet, Path).

% Process neighbors
process_neighbors(_, _, [], _, _, _, OpenHeap, _, OpenHeap).
process_neighbors(Map, Goal, [Neighbor|Rest], Current, Cost, PathSoFar, OpenHeap, ClosedSet, FinalHeap) :-
    % Calculate new cost
    get_map_tile(Map, Neighbor, Tile),
    get_tile_cost(Tile, TileCost),
    NewCost is Cost + TileCost,
    
    % Calculate f-score
    heuristic(Neighbor, Goal, H),
    F is NewCost + H,
    
    % Add to open set if not in closed set
    (get_assoc(Neighbor, ClosedSet, _) ->
        NextHeap = OpenHeap
    ;
        append(PathSoFar, [Neighbor], NewPath),
        add_to_heap(OpenHeap, F, (Neighbor,NewCost,NewPath), NextHeap)
    ),
    
    % Process remaining neighbors
    process_neighbors(Map, Goal, Rest, Current, Cost, PathSoFar, NextHeap, ClosedSet, FinalHeap).

% Find total path cost
find_path_cost(Path, Map, TotalCost) :-
    path_cost(Path, Map, 0, TotalCost).

path_cost([_], _, Cost, Cost) :- !.
path_cost([Pos1,Pos2|Rest], Map, AccCost, TotalCost) :-
    get_map_tile(Map, Pos2, Tile),
    get_tile_cost(Tile, TileCost),
    NewAccCost is AccCost + TileCost,
    path_cost([Pos2|Rest], Map, NewAccCost, TotalCost).

% Example usage:
% ?- Map = [[water, land, forest], [land, water, land], [forest, land, water]], 
%    find_path(Map, (0,0), (2,2), Path).