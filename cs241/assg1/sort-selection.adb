with Ada.Real_Time; use Ada.Real_Time;

package body Sort.Selection is

	procedure SelectionSort(unsortedFile : in out File_Type; 
							sortedFile : in out File_Type; 
							infoFile : in out File_Type)
	is
		length : Natural;
		startTime, endTime : Time;
		diagnostics : SortDiagnostics;
	begin
		NumNaturals(unsortedFile, length);

		-- Set up the timer.
		delay 0.0;
		startTime := Clock;

		-- Set up the diagnostics
		diagnostics.NumNaturals := Long_Long_Integer(length);
		diagnostics.TypeOfSort := Selection_Sort;
		
		-- Declare block to allow dynamic size of array.
		TimeToSort:
		declare
			data : NatArray(1..length);
			min : Natural;
			minPos : Integer;

		begin
			GetNaturals(data, unsortedFile);

			for i in data'range loop
				-- One write.
				diagnostics.NumWrites := diagnostics.NumWrites + 1;
				min := data(i);

				for j in (i +1) .. data'last loop

					-- One Comparison
					diagnostics.NumComparisons := diagnostics.NumComparisons + 1;
					if(data(j) < min) then
						-- Two Writes
						diagnostics.NumWrites := diagnostics.NumWrites + 2;
						min := data(j);
						minPos := j;
					end if;
				end loop;

				-- One Comparison
				diagnostics.NumComparisons := diagnostics.NumComparisons +1;
				if(min /= data(i)) then
					-- One Exchange
					diagnostics.NumExchanges := diagnostics.NumExchanges + 1;
					Exchange(data, i, minPos);
				end if;
			end loop;
			
			-- Clock the timer before writing the sorted array
			endTime := Clock;
			
			-- Write the sorted array to the file while it is still declared.
			WriteSorted(sortedFile, data);
		end TimeToSort;

		-- Calculate running time with before and after times
		diagnostics.RunningTime := Float(To_Duration(endTime - startTime));

		-- Write Diagnostics now that sort is complete.
		WriteDiagnostics(infoFile, diagnostics);
		DisplayDiagnostics(diagnostics);

	end SelectionSort;

end Sort.Selection;
