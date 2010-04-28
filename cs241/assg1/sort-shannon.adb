with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

package body Sort.Shannon is

	procedure ShannonSort(unsortedFile : in out File_Type; 
						  sortedFile : in out File_Type; 
						  infoFile : in out File_Type;
						  gapFile : in out File_Type)
	is
		length : Natural;
		gapLength : Natural;
		startTime, endTime : Time;
		diagnostics : SortDiagnostics;
	begin
		-- Get number of naturals in files to declare arrays of that size
		NumNaturals(unsortedFile, length);
		NumNaturals(gapFile, gapLength);

		-- Set up the timer.
		delay 0.0;
		startTime := Clock;

		-- Set up diagnostics
		diagnostics.NumNaturals := Long_Long_Integer(length);
		diagnostics.TypeOfSort := Shannon_Sort;


		-- Declare block to create an array with dynamic size
		TimeToSort:
		declare
			data : NatArray(1..length);
			rowSequence : NatArray(1..gapLength);
			rows : Natural;
			size : Natural;
			startIndex : Natural;
			endIndex : Natural;
			min : Natural;
			minIndex : Natural;

		begin
			GetNaturals(data, unsortedFile);
			GetNaturals(rowSequence, gapFile);
	
			-- For each number in the gap sequence
			for r in rowSequence'range loop

				-- One write.
				diagnostics.NumWrites := diagnostics.NumWrites +1;

				rows := rowSequence(r);

				-- Only sort if there are less rows than the size of the array
				-- One Comparison
				diagnostics.NumComparisons := diagnostics.NumComparisons +1;

				if (rows < data'length) then
					
					-- One Write
					diagnostics.NumWrites := diagnostics.NumWrites + 1;
					size := data'length / rows;

					-- Sort each row
					for row in 1..rows loop

						-- Calculate start/end index for this row
						-- Two Writes
						diagnostics.NumWrites := diagnostics.NumWrites + 2;
						startIndex := data'first + (row-1)*size;
						endIndex := data'first + row*size;

						-- The gap probably won't divide the array evenly, so on the last row, we should take
						-- the last element to be the last index
						-- One Comparison.
						diagnostics.NumComparisons := diagnostics.NumComparisons +1;
						if(endIndex > data'last) then
							-- One Write
							diagnostics.NumWrites := diagnostics.NumWrites + 1;
							endIndex := data'last;
						end if;

						-- Now do a selection sort.
						for i in startIndex..endIndex loop

							-- One Write
							diagnostics.NumWrites := diagnostics.NumWrites + 1;
							min := data(i);

							for j in (i +1)..endIndex loop
								
								-- One Comparison.
								diagnostics.NumComparisons := diagnostics.NumComparisons +1;
								if(data(j) < min) then

									-- Two Writes.
									diagnostics.NumWrites := diagnostics.NumWrites + 2;
									min := data(j);
									minIndex := j;
								end if;
							end loop;
							
							-- One Comparison.
							diagnostics.NumComparisons := diagnostics.NumComparisons +1;
							if(min /= data(i)) then

								-- One Exchange.
								diagnostics.NumExchanges := diagnostics.NumExchanges + 1;
								Exchange(data, i, minIndex);
							end if;

						end loop;

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

	end ShannonSort;

end Sort.Shannon;
