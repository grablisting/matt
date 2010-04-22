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
			j : Natural;
			tmp : Natural;
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

				    -- Do an insertion sort on just these numbers
				for i in gap..(data'last -1) loop
				    tmp := data(i);
				    j := i+1;
				    while (j >= gap and then data(j - gap) > tmp) loop
					put_line(Integer'Image(j)); 
					data(j) := data(j - gap);
					j := j - gap;
				    end loop;
				    data(j) := tmp;
				end loop;
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
