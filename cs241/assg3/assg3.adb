-- Matt Forbes
-- CSCI 241 - Assignment 3
-- Dijkstra's Shortest Path Algorithm

with Ada.Text_IO; use Ada.Text_IO;
with Easy_IO; use Easy_IO;
with Graphs; use Graphs;
with Graphs.Dijkstra; use Graphs.Dijkstra;

procedure Assg3 
is
    graph : WeightedGraph;
    result : DijkstraResult;
begin
    graph := ReadWeightedGraphFromFile(GetInputFile("Input file containing weighted graph: "));
    result := RunDijkstras(graph);
    DisplayDijkstraResult(result);
    WriteDijkstraResult(GetOutputFile("Output file to write result of Dijkstra's algorithm to: "), result);
end Assg3;
