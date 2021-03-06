-- Matt Forbes
-- CSCI 241 - Assignment 3
-- Dijkstra's Shortest Path Algorithm
-- Main program - driver

with Ada.Text_IO; use Ada.Text_IO;
with Easy_IO; use Easy_IO;
with Graphs; use Graphs;
with Graphs.Dijkstra; use Graphs.Dijkstra;

procedure Assg3 
is
    graph : WeightedGraph;
    result : DijkstraResult;
    inFile, outFile : File_Type;
begin
    GetInputFile("Input file containing weighted graph: ", inFile);
    GetOutputFile("Output file to write result of Dijkstra's algorithm to: ", outFile);

    -- get the graph from file
    ReadWeightedGraphFromFile(inFile, graph);

    -- run dijkstra's algorithm on the graph
    result := RunDijkstras(graph);

    -- display and print graph representation to output file
    DisplayWeightedGraph(graph);
    WriteWeightedGraph(outFile, graph);

    -- display and print dijkstra's results to output file
    WriteDijkstraResult(outFile, result);
    DisplayDijkstraResult(result);

    -- cleanup
    Close(inFile);
    Close(outFile);
    Free(result);
    Free(graph);
end Assg3;
