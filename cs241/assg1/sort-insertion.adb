with Ada.Real_Time; use Ada.Real_Time;

package body Sort.Insertion is

	procedure InsertionSort(unsortedFile : in out File_Type; 
							sortedFile : in out File_Type; 
							infoFile : in out File_Type)
	is
		length : Natural;
		diagnostics : SortDiagnostics;
		startTime, endTime : Time;
	begin
		-- Get number of naturals in file to declare a array of that size
		NumNaturals(unsortedFile, length);

		-- Set up the timer.
		delay 0.0;
		startTime := Clock;

		-- Set up diagnostics
		diagnostics.NumNaturals := length;
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
			
			for i in (data'first +1)..(data'last -1) loop
				-- Two Writes here.
				diagnostics.NumWrites := diagnostics.NumWrites + 2;
				tmp := data(i);
				j := i-1;

				loop
					-- One Comparison here.
					diagnostics.NumComparisons := diagnostics.NumComparisons + 1;
					if(data(j) > tmp) then

						-- Two Writes here.
						diagnostics.NumWrites := diagnostics.NumWrites + 2;
						data(j+1) := data(j);
						j := j-1;

						-- One Comparison here.
						diagnostics.NumComparisons := diagnostics.NumComparisons + 1;
						if(j < data'first) then
							exit;
						end if;
					else
						exit;
					end if;
				end loop;

				-- One Write here.
				diagnostics.NumWrites := diagnostics.NumWrites + 1;
				data(j+1) := tmp;
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
