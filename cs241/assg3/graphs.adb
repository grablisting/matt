with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
package body Graphs is
    
    --------------------
    -- Memory Management
    procedure Deallocate_WeightedGraph is new Ada.Unchecked_Deallocation(WeightedGraph_Type, WeightedGraph);
    procedure Deallocate_WeightArray is new Ada.Unchecked_Deallocation(WeightArray_Type, WeightArray);

    procedure Free(path : in out GraphPath)
    is
    begin
	Path_List.clear(path.head);
    end Free;

    procedure Free(graph : in out WeightedGraph)
    is
    begin
	Deallocate_WeightArray(graph.Weights);
	Deallocate_WeightedGraph(graph);
    end Free;
    --------------------
    -- Memory Management

    --------------------
    -- I/O Methods
    procedure WriteWeightedGraph(file : in File_Type; graph : in WeightedGraph)
    is
    begin
	null;
    end WriteWeightedGraph;

    procedure DisplayWeightedGraph(graph : in WeightedGraph)
    is
    begin
	WriteWeightedGraph(Standard_Input, graph);
    end DisplayWeightedGraph;

    function ReadWeightedGraphFromFile(fileName : string) return WeightedGraph
    is
    begin
	return new WeightedGraph_Type;
    end ReadWeightedGraphFromFile;

    --------------------
    -- I/O Methods


    --------------------
    -- Path Methods
    procedure AddNode(path : in out GraphPath; node : in GraphNode)
    is
    begin
	null;
    end AddNode;

    function ToString(path : in GraphPath) return String
    is
    begin
	return "";
    end ToString;
    --------------------
    -- Path Methods

   	

    
end Graphs;
