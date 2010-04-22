with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body Sort.Shell is

	procedure ShellSort(unsortedFile : in out File_Type; 
							sortedFile : in out File_Type; 
							infoFile : in out File_Type;
							gapFile : in out File_Type)
	is
		length : Natural;
		gapLength : Natural;
		diagnostics : SortDiagnostics;
		startTime, endTime : Time;
	begin
		-- Get number of naturals in files to declare arrays of that size
		NumNaturals(unsortedFile, length);
		NumNaturals(gapFile, gapLength);

		-- Set up the timer.
		delay 0.0;
		startTime := Clock;

		-- Set up diagnostics
		diagnostics.NumNaturals := length;
		diagnostics.TypeOfSort := Shell_Sort;


		-- Declare block to create an array with dynamic size
		TimeToSort:
		declare
			data : NatArray(1..length);
			gapSequence : NatArray(1..gapLength);
			sortIndex : Natural;
			numSorting : Natural;
			min : Natural;
			minIndex : Natural;
			gap : Natural;

		begin
			GetNaturals(data, unsortedFile);
			GetNaturals(gapSequence, gapFile);
	
			-- For each number in the gap sequence
			for g in gapSequence'range loop
				
				-- Store the gap. One write.
				diagnostics.NumWrites := diagnostics.NumWrites + 1;
				gap := gapSequence(g);

				-- Only sort on numbers in the sequence that are less than the length of the array
				-- One Comparison
				diagnostics.NumComparisons := diagnostics.NumComparisons + 1;

				if (gap < data'length) then
					
					-- One Write.
					diagnostics.NumWrites := diagnostics.NumWrites + 1;

					numSorting := (data'length / gap);

					-- Do a selection sort on just these numbers
					for i in 1..numSorting loop
						
						-- Two writes here.
						diagnostics.NumWrites := diagnostics.NumWrites + 2;

						sortIndex := i*gap;

						-- Set initial minimum to the first index of the array
						min := data(sortIndex);

						for j in (i +1)..numSorting loop
							
							-- One Comparion
							diagnostics.NumComparisons := diagnostics.NumComparisons + 1;
					
							if(data(j*gap) < min) then

								-- Two Writes.
								diagnostics.NumWrites := diagnostics.NumWrites + 2;

								min := data(j*gap);
								minIndex := j*gap;

							end if;
						end loop;

						-- One Comparion
						diagnostics.NumComparisons := diagnostics.NumComparisons + 1;

						if(min /= data(sortIndex)) then
							-- One Exchange
							diagnostics.NumExchanges := diagnostics.NumExchanges + 1;

							Exchange(data, minIndex, sortIndex);
						end if;
					end loop;

				end if;
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

	end ShellSort;

end Sort.Shell;
