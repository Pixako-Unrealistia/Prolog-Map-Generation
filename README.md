# Project Description
The project is to create a tile map procedural generation then apply a search algorithm to create road,
Users can configure how much percentage of individual tiles they want the world to be, alongside the size of the map. 
Possible tiles are configurable by the user alongside their constraints of invalid nearest neighbour and required nearest neighbour
Procedural generation starts with perlin noise sampling to create a base map, then correction algorithm will paint over the map depending on the user configuration
If there is no such way to configure, corrections will not be made and leave that specific area as is.
Additionally, a third available option would be a seeded map, where the user may draw the map they wish and the rest of the tiles will be procedurally filled per given threshold

After the map is generated, the user can select an option to draw a road between two tiles. The cost of each tile will be configured in the tile editor, and the least costly path will be drawn from start to finish.

# The AI Concepts to Apply in this Project
## Rule-based
Rule-based systems are a fundamental concept for this map generation project. In this context, a rule-based system would be used to govern tile placement and correction based on predefined rules that reflect the desired environmental logic. These rules capture configurable constraints and relationships between different tile types, for instance:
The rule-based approach allows for easy modification of rules, enabling quick adjustments to the map generation logic and experimentation with different environmental constraints. The implementation can be broken down into:
Rule Definition: Clearly defining each rule and its conditions (e.g., "Ocean must be next to sand").
Rule Application: Implementing mechanisms to apply these rules during tile placement, ensuring that each new tile adheres to the defined constraints.
Conflict Resolution: Developing strategies to handle situations where multiple rules conflict or a perfect solution is impossible, potentially through user prompts or automated correction mechanisms 

## Path finding
Once the map is generated, pathfinding algorithms are employed to:
Calculate shortest paths: Determine the shortest path from a selected tile (potential village location) to the nearest source of "fresh water" (presumably represented by a specific tile type) and forest.
Estimate travel cost: Assign "costs" to traversing different tile types, reflecting the difficulty or time required to travel through them (e.g., higher cost for water tiles).

## Decision making
Start tile from red, available moves are west, south, north,east, north east, north west, south east and south west. The distance drawn is based on cost of individual tile combined with node depth;

