:- consult('tile_sets.pl').

invalid_tile(Tile, Neighbors) :-
    tile_set(Tile, _, CannotBeNextTo, _, _, _, _),
    member(Neighbor, Neighbors),
    member(Neighbor, CannotBeNextTo).

invalid_tile(Tile, Neighbors) :-
    tile_set(Tile, _, _, MustBeNextTo, _, _, _),
    MustBeNextTo \= [],
    \+ (member(Neighbor, Neighbors), member(Neighbor, MustBeNextTo)).
