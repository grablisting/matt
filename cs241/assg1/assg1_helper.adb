with Options; use Options;
with Ada.Text_IO; use Ada.Text_IO;
with Sort; use Sort;
with Sort.Selection; use Sort.Selection;
with Sort.Insertion; use Sort.Insertion;
with Sort.Shannon; use Sort.Shannon;
with Sort.Shell; use Sort.Shell;

package body Assg1_Helper is

	debug : constant Boolean := false;

	procedure HandleOption(option : in OptionName)
	is

		unsortedFile, sortedFile, infoFile, gapFile : File_Type;
		
	begin
	    case option is
			when SelectionSort =>
				put_line("Starting Selection Sort");
				OpenSortFiles(unsortedFile, sortedFile, infoFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				CloseSortFiles(unsortedFile, sortedFile, infoFile);
				put_line("Selection Sort Complete");

			when InsertionSort =>
				put_line("Starting Insertion Sort");
				OpenSortFiles(unsortedFile, sortedFile, infoFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				CloseSortFiles(unsortedFile, sortedFile, infoFile);
				put_line("Insertion Sort Complete");

			when ShannonSort =>
				put_line("Starting Shannon Sort");
				OpenSortFiles(unsortedFile, sortedFile, infoFile, gapFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				CloseSortFiles(unsortedFile, sortedFile, infoFile, gapFile);
				put_line("Shannon Sort Complete");

			when ShellSort =>
				put_line("Starting Shell Sort");
				OpenSortFiles(unsortedFile, sortedFile, infoFile, gapFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				CloseSortFiles(unsortedFile, sortedFile, infoFile, gapFile);
				put_line("Shell Sort Complete");

			when GenerateDiagnostics =>
				put_line("Generating Diagnostics");
				begin
					Open(infoFile, Append_File, "dat/info.dat");
				exception
					when others => Create(infoFile, Append_File, "dat/info.dat");
				end;

				begin
					Open(sortedFile, Out_File, "dat/out.dat");
				exception
					when others => Create(sortedFile, Out_File, "dat/out.dat");
				end;

				-- Open Gap File
				Open(gapFile, In_File, "dat/gap.dat");

				---------------------------------------------------
				-- RANDOM ORDER
				--------------------------------------------------
				
				-- Insertion Sort
				new_line(infoFile);
				put_line(infoFile, "RANDOM INSERTION SORT----------");
				Open(unsortedFile, In_File, "dat/Random1000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Random100000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Random1000000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				--Selection Sort
				new_line(infoFile);
				put_line(infoFile, "RANDOM SELECTION SORT----------");
				Open(unsortedFile, In_File, "dat/Random1000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Random100000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Random1000000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				-- Shell Sort
				new_line(infoFile);
				put_line(infoFile, "RANDOM SHELL SORT----------");
				Open(unsortedFile, In_File, "dat/Random1000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Random100000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Random1000000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				-- Shannon Sort
				new_line(infoFile);
				put_line(infoFile, "RANDOM SHANNON SORT----------");
				Open(unsortedFile, In_File, "dat/Random1000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Random100000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Random1000000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);


				---------------------------------------------------
				-- Sorted ORDER
				--------------------------------------------------
				
				--Insertion Sort
				new_line(infoFile);
				put_line(infoFile, "SORTED INSERTION SORT----------");
				Open(unsortedFile, In_File, "dat/Sorted0000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Sorted00000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Sorted000000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				-- Selection Sort
				new_line(infoFile);
				put_line(infoFile, "SORTED SELECTION SORT----------");
				Open(unsortedFile, In_File, "dat/Sorted0000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Sorted00000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Sorted000000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				-- Shell Sort
				new_line(infoFile);
				put_line(infoFile, "SORTED SHELL SORT----------");
				Open(unsortedFile, In_File, "dat/Sorted0000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Sorted00000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Sorted000000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				-- Shannon Sort
				new_line(infoFile);
				put_line(infoFile, "SORTED SHANNON SORT----------");
				Open(unsortedFile, In_File, "dat/Sorted0000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Sorted00000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Sorted000000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);
	
				---------------------------------------------------
				-- REVERSE ORDER
				--------------------------------------------------
			
				-- Insertion Sort
				new_line(infoFile);
				put_line(infoFile, "REVERSE INSERTION SORT----------");
				Open(unsortedFile, In_File, "dat/Reverse0000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Reverse00000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Reverse000000.dat");
				Reset(sortedFile);
				InsertionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				-- Selection Sort
				new_line(infoFile);
				put_line(infoFile, "REVERSE SELECTION SORT----------");
				Open(unsortedFile, In_File, "dat/Reverse0000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Reverse00000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Reverse000000.dat");
				Reset(sortedFile);
				SelectionSort(unsortedFile, sortedFile, infoFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);
				
				-- Shell Sort
				new_line(infoFile);
				put_line(infoFile, "REVERSE SHELL SORT----------");
				Open(unsortedFile, In_File, "dat/Reverse0000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Reverse00000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Reverse000000.dat");
				Reset(sortedFile);
				ShellSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				-- Shannon Sort
				new_line(infoFile);
				put_line(infoFile, "REVERSE SHANNON SORT----------");
				Open(unsortedFile, In_File, "dat/Reverse0000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Reverse00000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Open(unsortedFile, In_File, "dat/Reverse000000.dat");
				Reset(sortedFile);
				ShannonSort(unsortedFile, sortedFile, infoFile, gapFile);
				new_line(infoFile);
				Close(unsortedFile);
				new_line(infoFile);

				Close(infoFile);
				Close(gapFile);
				Close(sortedFile);
				
			when Others =>
				skip_line;
				put_line("Invalid Option");
				DisplayOptions;
		end case;
	end HandleOption;
    
	procedure OpenSortFiles(unsortedFile : in out File_Type;
						    sortedFile : in out File_Type;
							infoFile : in out File_Type)
	is
		fileName : String(1..30);
		length : Natural;
	begin
		loop
			begin
				if(not debug) then
					put("Enter file name containing the unsorted integers: ");
					get_line(filename, length);
					Open(unsortedFile, In_File, fileName(1..length));
				else
					Open(unsortedFile, In_File, "dat/Random1000000.dat");
				end if;
				exit;
			exception
				when others => put_line("Could not open file");
			end;
		end loop;

		loop
			begin
				if(not debug) then
					put("Enter file name to write the sorted integers to: ");
					get_line(filename, length);
					Open(sortedFile, Out_File, fileName(1..length));
				else
					Open(sortedFile, Out_File, "dat/out.dat");
				end if;
				exit;
			exception
				when others =>
					begin
						Create(sortedFile, Out_File, fileName(1..length));
						exit;
					exception
						when others => put_line("Could not create file");
					end;
			end;
		end loop;

		loop
			begin
				if(not debug) then
					put("Enter file name to write the diagnostics to: ");
					get_line(filename, length);
					Open(infoFile, Out_File, fileName(1..length));
				else
					Open(infoFile, Out_File, "dat/info.dat");
				end if;
				exit;
			exception
				when others =>
					begin
						Create(infoFile, Out_File, fileName(1..length));
						exit;
					exception
						when others => put_line("Could not create file");
					end ;
			end;
		end loop;
	end OpenSortFiles;

	procedure CloseSortFiles(unsortedFile : in out File_Type; 
							 sortedFile : in out File_Type; 
							 infoFile : in out File_Type)
	is
	begin
		Close(unsortedFile);
		Close(sortedFile);
		Close(infoFile);
	end CloseSortFiles;


	procedure OpenSortFiles(unsortedFile : in out File_Type;
							sortedFile : in out File_Type;
							infoFile : in out File_Type;
							gapFile : in out File_Type)
	is
		fileName : String(1..30);
		length : Natural;

	begin
		OpenSortFiles(unsortedFile, sortedFile, infoFile);

		loop
			begin
				if(not debug) then
					put("Enter file name containing the gap sequence: ");
					get_line(filename, length);
					Open(gapFile, In_File, fileName(1..length));
				else
					Open(gapFile, In_File, "dat/gap.dat");
				end if;
				exit;
			exception
				when others => put_line("Could not open file");
			end;
		end loop;

	end OpenSortFiles;

	procedure CloseSortFiles(unsortedFile : in out File_Type; 
							 sortedFile : in out File_Type; 
							 infoFile : in out File_Type;
						   	 gapFile : in out File_Type)
	is
	begin
		CloseSortFiles(unsortedFile, sortedFile, infoFile);
		Close(gapFile);
	end CloseSortFiles;

end Assg1_Helper;
