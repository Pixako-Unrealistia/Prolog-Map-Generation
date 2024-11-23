:- consult('tile_sets.pl').

invalid_tile(Tile, Neighbors) :-
	tile_set(Tile, _, CannotBeNextTo, _, _, _, _),
	member(Neighbor, Neighbors),
	member(Neighbor, CannotBeNextTo).

invalid_tile(Tile, Neighbors) :-
	tile_set(Tile, _, _, MustBeNextTo, _, _, _),
	MustBeNextTo \= [],
	\+ (member(Neighbor, Neighbors), member(Neighbor, MustBeNextTo)).

must_be_next_to(Tile, Neighbors) :-
	tile_set(Tile, _, _, MustBeNextToList, _, _, _),
	( MustBeNextToList = [] ->
		true
	;
		( member(Neighbor, Neighbors),
		member(Neighbor, MustBeNextToList) )
	).

border_invalid(BorderTile, Neighbors) :-
	tile_set(BorderTile, _, BorderTileCannotBeNextTo, BorderTileMustBeNextTo, _, _, _),
	(
		( member(Neighbor, Neighbors),
		member(Neighbor, BorderTileCannotBeNextTo) )
	;
		( BorderTileMustBeNextTo \= [],
		\+ ( member(Neighbor, Neighbors),
			member(Neighbor, BorderTileMustBeNextTo) ) )
	).
