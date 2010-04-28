with Ada.Real_Time; use Ada.Real_Time;

package body Sort.Insertion is

	procedure InsertionSort(unsortedFile : in out File_Type; 
							sortedFile : in out File_Type; 
							infoFile : in out File_Type)
	is
		length : Natural;
		startTime, endTime : Time;
		diagnostics : SortDiagnostics;
	begin
		-- Get number of naturals in file to declare a array of that size
		NumNaturals(unsortedFile, length);

		-- Set up the timer.
		delay 0.0;
		startTime := Clock;

		-- Set up diagnostics
		diagnostics.NumNaturals := Long_Long_Integer(length);
		diagnostics.TypeOfSort := Insertion_Sort;

		-- Declare block to create an array with dynamic size
		TimeToSort:
		declare
			data : NatArray(1..length);
			tmp : Natural;
			j : Integer;
		begin
			-- Get the naturals to sort.
			GetNaturals(data, unsortedFile);
			
			for i in reverse data'range loop
			    diagnostics.NumWrites := diagnostics.NumWrites + 2;
			    j := i;
			    tmp := data(j);
			    while j < data'last and then data(j+1) < tmp loop
				diagnostics.NumComparisons := diagnostics.NumComparisons+2;
				diagnostics.NumWrites := diagnostics.NumWrites + 2;
				data(j) := data(j+1);
				j := j + 1;
			    end loop;
			    diagnostics.NumWrites := diagnostics.NumWrites + 1;
			    data(j) := tmp;
			end loop;

			-- Clock the timer before writing the sorted array
			endTime := Clock;


			-- Write the sorted file while we have the sorted array declared.
			WriteSorted(sortedFile, data);
		end TimeToSort;
	
		-- Calculate running time with before and after times
		diagnostics.RunningTime := Float(To_Duration(endTime - startTime));

		-- Write the diagnostics now that we've finished the sort.
		WriteDiagnostics(infoFile, diagnostics);
		DisplayDiagnostics(diagnostics);

	end InsertionSort;

end Sort.Insertion;
