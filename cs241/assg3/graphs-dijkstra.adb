with Ada.Text_IO; use Ada.Text_IO;
With Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body Graphs.Dijkstra is

    -------------------
    -- Memory Management
    procedure Deallocate_PathTable is new Ada.Unchecked_Deallocation(PathTable_Type, PathTable);
    procedure Deallocate_LengthTable is new Ada.Unchecked_Deallocation(LengthTable_Type, LengthTable);
    procedure Deallocate_VisitedArray is new Ada.Unchecked_Deallocation(VisitedArray_Type, VisitedArray);

    procedure Free(result : in out DijkstraResult)
    is
    begin
	Deallocate_LengthTable(result.Lengths);
	Deallocate_PathTable(result.Paths);
    end Free;
    -- Memory Management
    -------------------

    -------------------
    -- Main Methods
    function RunDijkstras(graph : WeightedGraph) return DijkstraResult
    is
	result : DijkstraResult;
	visited : VisitedArray;
	weight : Integer;
	nextWeight, oldWeight : Integer;
	nextNode : GraphNode;
    begin
	result.Lengths := new LengthTable_Type(1..graph.Size, 1..graph.Size);
	result.Paths := new PathTable_Type(1..graph.Size);

	result.Paths(1) := newGraphPath(1);
	
	-- initialize lengths/paths
	for i in 1..graph.Size loop
	    weight := graph.Weights(i,1);
	    result.Lengths(i,1) := weight;

	    if(weight /= -1 and i /= 1) then
		result.Paths(i) := result.Paths(1) + i;
	    end if;
	end loop;

	visited := new VisitedArray_Type(1..graph.Size);
	visited.all := (others => false);

	for i in 2..graph.Size loop
	    nextNode := PickNextNode(i-1, result.Lengths, visited);
	    visited(nextNode) := true;
	    weight := result.Lengths(nextNode, i-1);

	    for j in 1..graph.Size loop
		nextWeight := graph.Weights(j, nextNode);
		oldWeight := result.Lengths(j, i-1);
		if( (j /= nextNode) and 
		    (not visited(j)) and
		    (nextWeight > 0) and 
		    ( (oldWeight = -1) or ( (weight + nextWeight) < oldWeight)) ) 
		then
		    result.Lengths(j, i) := (weight + nextWeight);
		    Free(result.Paths(j));
		    result.Paths(j) := result.Paths(nextNode) + j;
		else
		    result.Lengths(j, i) := result.Lengths(j, i-1);
		end if;
	    end loop;
	end loop;

	Deallocate_VisitedArray(visited);
	return result;
    end RunDijkstras;

    function PickNextNode(iteration : Natural; lengths : LengthTable; visited : VisitedArray) return GraphNode
    is
	minLength : GraphWeight := Integer'Last;
	minIndex : GraphNode;
	curLength : GraphWeight;

    begin
	for i in lengths'range(1) loop
	    curLength := lengths(i, iteration);
	    if( (not visited(i)) and then ((curLength > 0) and (curLength < minLength)) ) then
		minLength := curLength;
		minIndex := i;
	    end if;
	end loop;
	return minIndex;
    end PickNextNode;

    -- Main Methods
    -------------------

    -------------------
    -- I/O Methods
    procedure WriteDijkstraResult(file : in File_Type; result : DijkstraResult)
    is
	val : Integer;
    begin
	put_line(file, "Shortest Length to Each Node from N1");
	for i in result.Lengths'Range loop
	    put(file, i, 6);
	end loop;
	new_line(file);
	put(file, "     ");
	for i in 1..result.Lengths'Last-1 loop
	    put(file, "------");
	end loop;
	new_line(file);

	for i in result.Lengths'Range(1) loop
	    for j in result.Lengths'Range(2) loop
		val := result.Lengths(j, i);
		if(val = -1) then
		    put(file, "     -");
		else
		    put(file, val, 6);
		end if;
	    end loop;
	    new_line(file);
	end loop;

	new_line(file);

	put_line(file, "Shortest Path to Each node from N1");
	for i in result.Paths'Range loop
	    put(file, i, 0);
	    put(file, ": ");
	    put_line(file, ToString(result.Paths(i)));
	end loop;
	new_line(file);
	    
    end WriteDijkstraResult;

    procedure DisplayDijkstraResult(result : in DijkstraResult)
    is
    begin
	WriteDijkstraResult(Standard_Output, result);
    end DisplayDijkstraResult;
    -- I/O Methods
    -------------------


end Graphs.Dijkstra;
