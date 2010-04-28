-- Matt Forbes
-- CS 241 Assignment 1
-- Sort Package that contains utilities for all of the sort functions

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;

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
			when Unknown => put_line(infoFile, "Unknown Sort:");
		end case;
		new_line(infoFile);

		put(infoFile, "Sorted ");
		put(infoFile, diagnostics.NumNaturals, 0);
		put(infoFile, " Naturals.");
		new_line(infoFile);

		put_line(infoFile, "Type of Data: " & ToString(diagnostics.TypeOfData));

		put(infoFile, "Running Time: ");
		put(infoFile, diagnostics.RunningTime, 0, 3, 0);
		put_line(infoFile, " seconds");

		put(infoFile, "Comparisons: ");
		put(infoFile, diagnostics.NumComparisons, 0);
		new_line(infoFile);

		put(infoFile, "Writes: ");
		put(infoFile, diagnostics.NumWrites, 0);
		new_line(infoFile);

		put(infoFile, "Exchanges: ");
		put(infoFile, diagnostics.NumExchanges, 0);
		new_line(infoFile);

		put(infoFile, "Total Operations: ");
		put(infoFile, diagnostics.NumExchanges + diagnostics.NumWrites + diagnostics.NumComparisons, 0);
		new_line(infoFile);

	end WriteDiagnostics;

	procedure WriteDiagnosticsCSV(csvFile : in out File_Type; diagnostics : in SortDiagnostics)
	is
	begin
		null;
	end;
	
	procedure DisplayDiagnostics(diagnostics : in SortDiagnostics)
	is
	begin
		put("Diagnostics for " );
		case diagnostics.TypeOfSort is
			when Insertion_Sort => put_line("Insertion Sort:");
			when Selection_Sort => put_line("Selection Sort:");
			when Shannon_Sort => put_line("Shannon Sort:");
			when Shell_Sort => put_line("Shell Sort:");
			when Unknown => put_line("Unknown Sort:");
		end case;
		new_line;

		put("Sorted ");
		put(diagnostics.NumNaturals, 0);
		put(" Naturals.");
		new_line;

		put_line("Type of Data: " & ToString(diagnostics.TypeOfData));

		put("Running Time: ");
		put(diagnostics.RunningTime, 0, 3, 0);
		put_line(" seconds");

		put("Comparisons: ");
		put(diagnostics.NumComparisons, 0);
		new_line;

		put("Writes: ");
		put(diagnostics.NumWrites, 0);
		new_line;

		put("Exchanges: ");
		put(diagnostics.NumExchanges, 0);
		new_line;

		put("Total Operations: ");
		put(diagnostics.NumExchanges + diagnostics.NumWrites + diagnostics.NumComparisons, 0);
		new_line;

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

	function ToString(typeOfData : DataType) return String
	is
	begin
	    case typeOfData is
		when RandomOrder => return "Random Order";
		when SortedOrder => return "Sorted Order";
		when ReverseOrder => return "Reverse Order";
		when others => return "Unknown Order";
	    end case;
	end ToString;
	
end Sort;
