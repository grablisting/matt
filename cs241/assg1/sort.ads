with Ada.Text_IO; use Ada.Text_IO;

package Sort is
	type NatArray is Array(Integer range <>) of Natural; 
	type DataType is (RandomOrder, ReverseOrder, SortedOrder, Unknown);
	type SortType is (Insertion_Sort, Selection_Sort, Shannon_Sort, Shell_Sort, Unknown);
	type SortDiagnostics is
		record
			NumComparisons : Long_Long_Integer := 0;
			NumWrites : Long_Long_Integer := 0;
			NumExchanges : Long_Long_Integer := 0;
			NumNaturals : Long_Long_Integer := 0;
			RunningTime : Float := 0.0;
			TypeOfSort : SortType := Unknown;
			TypeOfData : DataType := Unknown;
		end record;

	procedure WriteDiagnostics(infoFile : in out File_Type; diagnostics : in SortDiagnostics); 	
	procedure WriteDiagnosticsCSV(csvFile : in out File_Type; diagnostics : in SortDiagnostics); 	
	procedure WriteSorted(sortedFile : in out File_Type; data : in NatArray);
	procedure DisplayDiagnostics(diagnostics : in SortDiagnostics);
	procedure NumNaturals(unsortedFile : in out File_Type; length : out Natural);
	procedure GetNaturals(data : in out NatArray; unsortedFile : in out File_Type);
	procedure Exchange(data : in out NatArray; index1 : in Natural; index2 : Natural);

	function ToString(typeOfData : DataType) return String;
	
end Sort;
