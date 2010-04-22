-- Matt Forbes
-- CS 241 Assignment 1
-- Sort Package that contains utilities for all of the sort functions

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Sort is

	procedure WriteDiagnostics(infoFile : in out File_Type; diagnostics : in SortDiagnostics)
	is
	begin
		put(infoFile, "Diagnostics for " );
		case diagnostics.TypeOfSort is
			when Insertion_Sort => put_line(infoFile, "Insertion Sort:");
			when Selection_Sort => put_line(infoFile, "Selection Sort:");
			when Shannon_Sort => put_line(infoFile, "Shannon Sort:");
			when Shell_Sort => put_line(infoFile, "Shell Sort:");
		end case;
		new_line(infoFile);
		put_line(infoFile, "Sorted " & Integer'Image(diagnostics.NumNaturals) & " Naturals.");
		put(infoFile, "Running Time: ");
		put(infoFile, diagnostics.RunningTime, 0, 3, 0);
		put_line(infoFile, " seconds");
		put_line(infoFile, "Comparisons: " & Integer'Image(diagnostics.NumComparisons));
		put_line(infoFile, "Writes: " & Integer'Image(diagnostics.NumWrites));
		put_line(infoFile, "Exchanges: " & Integer'Image(diagnostics.NumExchanges));
	end WriteDiagnostics;

	procedure DisplayDiagnostics(diagnostics : in SortDiagnostics)
	is
	begin
		put("Diagnostics for " );
		case diagnostics.TypeOfSort is
			when Insertion_Sort => put_line("Insertion Sort:");
			when Selection_Sort => put_line("Selection Sort:");
			when Shannon_Sort => put_line("Shannon Sort:");
			when Shell_Sort => put_line("Shell Sort:");
		end case;
		new_line;
		put_line("Sorted " & Integer'Image(diagnostics.NumNaturals) & " Naturals.");
		put("Running Time: ");
		put(diagnostics.RunningTime, 0, 3, 0);
		put_line(" seconds");
		put_line("Comparisons: " & Integer'Image(diagnostics.NumComparisons));
		put_line("Writes: " & Integer'Image(diagnostics.NumWrites));
		put_line("Exchanges: " & Integer'Image(diagnostics.NumExchanges));
	end DisplayDiagnostics;


	procedure WriteSorted(sortedFile : in out File_Type; data : in NatArray) 
	is
	begin
		Reset(sortedFile);
		for i in data'range loop
			put_line(sortedFile, Integer'Image(data(i)));
		end loop;
	end WriteSorted;
	
	procedure NumNaturals(unsortedFile : in out File_Type; length : out Natural)
	is
		fake : Natural;
	begin
		length := 0;
		Reset(unsortedFile);
		loop
			exit when end_of_file(unsortedFile);
			get(unsortedFile, fake);
			length := length + 1;
		end loop;
	end NumNaturals;

	procedure GetNaturals(data : in out NatArray; unsortedFile : in out File_Type) 
	is
	begin
		Reset(unsortedFile);
		for i in data'range loop
			get(unsortedFile, data(i));			
		end loop;
	end GetNaturals;

	procedure Exchange(data : in out NatArray; index1 : in Natural; index2 : Natural)
	is
		tmp: Natural;
	begin
		tmp := data(index1);
		data(index1) := data(index2);
		data(index2) := tmp;
	end Exchange;

end Sort;
