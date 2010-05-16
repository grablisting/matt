with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Generic_Linked_List;

package Graphs is

    -- type declarations
    subtype GraphWeight is Integer range -1..Integer'Last;
    subtype GraphNode is Natural;
    type WeightedGraph is private;
    type GraphPath is private;

    -- i/o methods
    procedure ReadWeightedGraphFromFile(file : in out File_Type; graph : out WeightedGraph);
    procedure WriteWeightedGraph(file : in File_Type; graph : in WeightedGraph);
    procedure DisplayWeightedGraph(graph : in WeightedGraph);
    
    -- memory management
    procedure Free(graph : in out WeightedGraph);

private

    -- internal type declarations
    type WeightArray_Type is Array(Natural range <>, Natural range <>) of GraphWeight;
    type WeightArray is access WeightArray_Type;

    package Path_List is new Generic_Linked_List(GraphNode);
    use type Path_List.listPtr;

    -- simple linked list that holds it's head and last pointer
    type GraphPath is
	record
	    head : Path_List.listPtr;
	    last : Path_List.listPtr;
	end record;
    
    -- representation of a weighted graph
    type WeightedGraph_Type is
	record
	    Weights : WeightArray;
	    Size : Natural;
	end record;

    type WeightedGraph is access WeightedGraph_Type;

    -- path methods
    function "+"(path : in GraphPath; node : in GraphNode) return GraphPath;
    function NewGraphPath(node : GraphNode) return GraphPath;
    function ToString(path : in GraphPath) return String;
    procedure Free(path : in out GraphPath);

	    
end Graphs;
