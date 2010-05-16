package Graphs.Dijkstra is

    type DijkstraResult is private;

    function RunDijkstras(graph : WeightedGraph) return DijkstraResult;
    procedure WriteDijkstraResult(file : in File_Type; result : DijkstraResult);
    procedure DisplayDijkstraResult(result : in DijkstraResult);
    procedure Free(result : in out DijkstraResult);

private
    
    type LengthTable_Type is Array(Natural range <>, Natural range <>) of Natural;
    type LengthTable is access LengthTable_Type;

    type PathTable_Type is Array(Natural range <>) of GraphPath;
    type PathTable is access PathTable_Type;

    type DijkstraResult is 
	record
	    lengths : LengthTable;
	    paths : PathTable;
	end record;
	    

end Graphs.Dijkstra;
