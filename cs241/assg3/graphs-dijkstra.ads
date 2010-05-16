package Graphs.Dijkstra is

    type DijkstraResult is private;

    function RunDijkstras(graph : WeightedGraph) return DijkstraResult;
    procedure WriteDijkstraResult(file : in File_Type; result : DijkstraResult);
    procedure DisplayDijkstraResult(result : in DijkstraResult);
    procedure Free(result : in out DijkstraResult);

private
    type VisitedArray_Type is Array(Natural range <>) of Boolean;
    type VisitedArray is access VisitedArray_Type;
    
    type LengthTable_Type is Array(Natural range <>, Natural range <>) of GraphWeight;
    type LengthTable is access LengthTable_Type;

    type PathTable_Type is Array(Natural range <>) of GraphPath;
    type PathTable is access PathTable_Type;

    type DijkstraResult is 
	record
	    lengths : LengthTable;
	    paths : PathTable;
	end record;

    function PickNextNode(iteration : Natural; lengths : LengthTable; visited : VisitedArray) return GraphNode;

end Graphs.Dijkstra;
