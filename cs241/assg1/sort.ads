with Ada.Text_IO; use Ada.Text_IO;

package Sort is
	type SortDiagnostics is private;
	type NatArray is Array(Integer range <>) of Natural; 

	procedure WriteDiagnostics(infoFile : in out File_Type; diagnostics : in SortDiagnostics); 	
	procedure WriteSorted(sortedFile : in out File_Type; data : in NatArray);
	procedure DisplayDiagnostics(diagnostics : in SortDiagnostics);
	procedure NumNaturals(unsortedFile : in out File_Type; length : out Natural);
	procedure GetNaturals(data : in out NatArray; unsortedFile : in out File_Type);
	procedure Exchange(data : in out NatArray; index1 : in Natural; index2 : Natural);
	
private
	type SortType is (Insertion_Sort, Selection_Sort, Shannon_Sort, Shell_Sort);
	type SortDiagnostics is
		record
			NumComparisons : Natural := 0;
			NumWrites : Natural := 0;
			NumExchanges : Natural := 0;
			NumNaturals : Natural := 0;
			RunningTime : Float := 0.0;
			TypeOfSort : SortType;
		end record;
end Sort;
