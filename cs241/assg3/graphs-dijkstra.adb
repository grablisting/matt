with Ada.Unchecked_Deallocation;

package body Graphs.Dijkstra is

    procedure Deallocate_PathTable is new Ada.Unchecked_Deallocation(PathTable_Type, PathTable);
    procedure Deallocate_LengthTable is new Ada.Unchecked_Deallocation(LengthTable_Type, LengthTable);

    procedure Free(result : in out DijkstraResult)
    is
    begin
	Deallocate_LengthTable(result.Lengths);
	Deallocate_PathTable(result.Paths);
    end Free;

    function RunDijkstras(graph : WeightedGraph) return DijkstraResult
    is
	result : DijkstraResult;
    begin
	return result;
    end RunDijkstras;

    procedure WriteDijkstraResult(file : in File_Type; result : DijkstraResult)
    is
    begin
	null;
    end WriteDijkstraResult;

    procedure DisplayDijkstraResult(result : in DijkstraResult)
    is
    begin
	WriteDijkstraResult(Standard_Input, result);
    end DisplayDijkstraResult;


end Graphs.Dijkstra;
