:- use_module(library(lists)).
:- consult('tile_sets.pl').

% Define the neighboring cells (up, down, left, right).
neighbors(X, Y, Width, Height, [(X1, Y), (X2, Y), (X, Y1), (X, Y2)]) :-
    X1 is X - 1, X1 >= 0,
    X2 is X + 1, X2 < Width,
    Y1 is Y - 1, Y1 >= 0,
    Y2 is Y + 1, Y2 < Height.

% Find a path between two points using A* search.
find_path(Map, Start, End, Path) :-
    width(Map, Width),
    height(Map, Height),
    astar(Map, Width, Height, Start, End, Path).

% A* search implementation.
astar(Map, Width, Height, Start, End, Path) :-
    astar_search([[(Start, 0, [])]], End, Width, Height, Map, RevPath),
    reverse(RevPath, Path).

astar_search([[(_, _, Path)|_]], End, _, _, _, Path) :-  % Goal reached
    Path = [(End, _) | _], !.

astar_search([[(_, Cost, Path)|_], End, Width, Height, Map, FullPath):-  % Only calculate path from this node
    Path = [(Current, _) | _],
    findall((Neighbor, NewCost, [(Current, Cost)|Path]),
       (expand_node(Map, Width, Height, Current, Cost, Neighbor, NewCost)),
            Neighbors
           ),
    insert_all(Neighbors, [], NewOpenList),
    astar_search(NewOpenList, End, Width, Height, Map, FullPath).

astar_search([_|Rest], End, Width, Height, Map, Path):- % Path blocked, try another
    astar_search(Rest, End, Width, Height, Map, Path).

expand_node(Map, Width, Height, (X, Y), Cost, (NX, NY), NewCost) :-
    neighbors(X, Y, Width, Height, Neighbors),
    member((NX, NY), Neighbors),
    \+ member((NX, NY), [(X,Y)]), % Avoid cycles by not considering previous node
    cell(Map, NX, NY, Tile),
    tile_set(Tile, TileCost, _, _, _, _, _),  % Get traversal cost from tile_sets.pl
    NewCost is Cost + TileCost.

insert_all([], L, L).
insert_all([(Node, Cost, Path)|T], L, Res) :-
    insert((Node, Cost, Path), L, L1),
    insert_all(T, L1, Res).

insert((Node, C, Path), [], [(_,C,Path)]):- !. % empty list case
insert(X, [H|T], [H|NT]):-
    H = (_, CostH, _),
    X = (_, CostX, _),
    CostH =< CostX, !, insert(X,T, NT). % sort based on F value

insert(X, [H|T], [X, H|T]).



% Get the width of the map.
width(Map, Width) :-
    Map = [Row|_],
    length(Row, Width).

% Get the height of the map.
height(Map, Height) :-
    length(Map, Height).

% Get the tile at a specific cell.
cell(Map, X, Y, Tile) :-
    nth0(Y, Map, Row),
    nth0(X, Row, Tile).

% Function to calculate total path cost
find_path_cost([], _, 0).
find_path_cost([(X,Y)|Rest], Map, TotalCost):-
    cell(Map, X, Y, Tile),
    tile_set(Tile, TileCost, _, _, _, _, _),
    find_path_cost(Rest, Map, RestCost),
    TotalCost is TileCost + RestCost.