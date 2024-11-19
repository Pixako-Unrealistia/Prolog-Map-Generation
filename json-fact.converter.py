import json
import sys


def json_to_prolog(json_file, prolog_file):
	with open(json_file, 'r') as f:
		tile_sets = json.load(f)

	with open(prolog_file, 'w') as f:
		for tile_set in tile_sets:
			name = tile_set['name']
			traversal_cost = tile_set['traversal_cost']
			cannot_be_next_to = tile_set['cannot_be_next_to']
			must_be_next_to = tile_set['must_be_next_to']
			color = tile_set['color']
			discoverable = 'true' if tile_set['discoverable'] else 'false'
			texture_path = tile_set['texture_path']

			cannot_be_next_to_str = '[' + ', '.join(cannot_be_next_to) + ']'
			must_be_next_to_str = '[' + ', '.join(must_be_next_to) + ']'

			fact = f"tile_set({name}, {traversal_cost}, {cannot_be_next_to_str}, {must_be_next_to_str}, '{color}', {discoverable}, '{texture_path}').\n"
			f.write(fact)


json_to_prolog('tile_sets.json', 'tile_sets.pl')