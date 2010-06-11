-- Matt Forbes
-- May 19, 2010
-- Subpackage of graphs that provides utilities to run and display the results
-- of running dijkstra's algorithm on a weighted graph
package Graphs.Dijkstra is

    -- type for encapsulating the shortest lengths/paths of a graph from one
    -- node to every other
    type DijkstraResult is private;

    -- given a graph, return a dijkstra result when it's algorithm is applied to that graph
    function RunDijkstras(graph : WeightedGraph) return DijkstraResult;

    -- I/O methods for printing results
    procedure WriteDijkstraResult(file : in File_Type; result : DijkstraResult);
    procedure DisplayDijkstraResult(result : in DijkstraResult);

    -- Cleanup methods
    procedure Free(result : in out DijkstraResult);

private
    -- Internal types for use in the algorithm. All arrays are used as access types
    -- to avoid passing entire structures between methods

    -- Array to store which nodes have been visited
    type VisitedArray_Type is Array(Natural range <>) of Boolean;
    type VisitedArray is access VisitedArray_Type;
    
    -- Array to store the length from the first node to every other at each step
    -- of dijkstra's algorithm
    type LengthTable_Type is Array(Natural range <>, Natural range <>) of GraphWeight;
    type LengthTable is access LengthTable_Type;

    -- Array to store the shorted path from the first node to every other at each step
    -- of dijkstra's algorithm
    type PathTable_Type is Array(Natural range <>) of GraphPath;
    type PathTable is access PathTable_Type;

    -- Private definition of dijkstra result, just has a length/path table
    type DijkstraResult is 
	record
	    lengths : LengthTable;
	    paths : PathTable;
	end record;

    -- private method to pick next node in a given step of the algorithm
    function PickNextNode(iteration : Natural; 
			  lengths : LengthTable; 
			  visited : VisitedArray) return GraphNode;

end Graphs.Dijkstra;
