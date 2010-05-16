with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
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
	val : Integer;
    begin
	put_line(file, "Graph Interpretation:");
	put(file, "      ");
	for i in 1..graph.Size loop
	    put(file, i, 6);
	end loop;
	new_line(file);

	for i in 1..graph.Size loop
	    put(file, i, 6);
	    for j in 1..graph.Size loop
		val := graph.Weights(i,j);
		if(val = -1) then
		    put(file, "     -");
		else
		    put(file, val, 6);
		end if;
	    end loop;
	    new_line(file);
	end loop;
    end WriteWeightedGraph;

    procedure DisplayWeightedGraph(graph : in WeightedGraph)
    is
    begin
	WriteWeightedGraph(Standard_Output, graph);
    end DisplayWeightedGraph;

    function GraphSize(file : in File_Type) return Natural
    is
	tmp : Natural;
	len : Natural := 0;
    begin
	while(not End_Of_Line(file)) loop
	    get(file, tmp);
	    len := len+1;
	end loop;
	return len;
    end GraphSize;

    procedure ReadWeightedGraphFromFile(file : in out File_Type; graph : out WeightedGraph)
    is
	size : Natural := GraphSize(file);
	val : Integer;
    begin
	graph := new WeightedGraph_Type;
	graph.Size := size;
	graph.Weights := new WeightArray_Type(1..size, 1..size);

	Reset(file);
	for i in 1..size loop
	    for j in (1+i)..size loop
		get(file, val);
		if(val = 0 and i /= j) then
		    val := -1;
		end if;
		graph.Weights(i,j) := val;
		graph.Weights(j,i) := val;
	    end loop;
	end loop;

	put_line("Done Reading Weighted Graph");

    end ReadWeightedGraphFromFile;

    --------------------
    -- I/O Methods


    --------------------
    -- Path Methods
    function "+"(path : in GraphPath; node : in GraphNode) return GraphPath
    is
	newPath : GraphPath := path;
    begin
	newPath.last := Path_List.insert_after(newPath.last, node);
	return newPath;
    
    end "+";


    function NewGraphPath(node : GraphNode) return GraphPath
    is
	newPath : GraphPath;
	ptr : Path_List.listPtr;
    begin
	ptr := Path_List.new_list(node);
	newPath.Head := ptr;
	newPath.Last := ptr;
	return newPath;
    end NewGraphPath;
    
    function ToString(path : in GraphPath) return String
    is
	iter : Path_List.listPtr := path.head;
	str : Unbounded_String;
    begin
	while(Path_List.next(iter) /= null) loop
	    Append(str, "N");
	    Append(str, Integer'Image(Path_List.value(iter)));
	    Append(str, " -> ");
	    iter := Path_List.next(iter);
	end loop;
	Append(str, "N");
	Append(str,Integer'Image(Path_List.value(iter)));
	return To_String(str);
    end ToString;
    --------------------
    -- Path Methods

   	

    
end Graphs;
