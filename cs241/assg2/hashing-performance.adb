with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body Hashing.Performance is

    use type Int_List.listPtr;

    procedure DeallocatePerformanceRecord is new Ada.Unchecked_Deallocation(PerformanceRecord_Type, PerformanceRecord);
    procedure Free(pRecord : in out PerformanceRecord)
    is
    begin
	DeallocatePerformanceRecord(pRecord);
    end Free;

    procedure AddProbes(pRecord : in out PerformanceRecord; probes : in Natural)
    is
    begin
	pRecord.NumProbes := pRecord.NumProbes + Long_Long_Integer(probes);
    end AddProbes;

    procedure AddInsertion(pRecord : in out PerformanceRecord)
    is
    begin
	pRecord.NumInsertions := pRecord.NumInsertions + 1;
    end AddInsertion;

    procedure AddLoadFactorStat(pRecord : in out PerformanceRecord; probes : in Natural)
    is
    begin
	Int_List.insert_last(pRecord.ProbesPerLoadFactor, probes);
    end AddLoadFactorStat;

    function LoadFactorGap(pRecord :  PerformanceRecord) return Integer
    is
    begin
	return pRecord.LoadFactorGap;
    end LoadFactorGap;

    procedure DisplayPerformanceRecord(pRecord : in out PerformanceRecord)
    is
	stdout : File_Type := Standard_Output;
    begin
	WritePerformanceRecord(pRecord, stdout);
    end DisplayPerformanceRecord;

    procedure WritePerformanceRecord(pRecord : in out PerformanceRecord; file : in out File_Type)
    is
	iter : Int_List.listPtr;
	currentLoadFactor : Integer := 0;
    begin
	put(file, "Performance for ");
	case pRecord.TypeOfProbing is
	    when LinearProbing => put(file, "linear");
	    when QuadraticProbing => put(file, "quadratic");
	    when CubicProbing => put(file, "cubic");
	    when BeatyProbing => put(file, "beaty");
	    when Others => put(file, "unknown");
	end case;
	put(file, "-probing hash table of size ");
	put_line(file, Integer'Image(pRecord.SizeOfHashTable));
	put(file, "Number of Probes: ");
	put(file, pRecord.NumProbes, 0);
	new_line(file);
	put(file, "Number of Insertions: ");
	put(file, pRecord.NumInsertions, 0);
	new_line(file);
	put(file, "Average Number of Probes per Insertion: ");
	put(file, (pRecord.NumProbes / pRecord.NumInsertions), 0);
	new_line(file);

	iter := pRecord.ProbesPerLoadFactor;
	while(iter /= null) loop
	    put(file, "Number of Probes at Load Factor ");
	    put(file, Float(currentLoadFactor) / 100.0, 0, 2, 0);
	    put(file, ": " & Integer'Image(Int_List.value(iter)));
	    new_line(file);
	    currentLoadFactor := currentLoadFactor + pRecord.LoadFactorGap;
	    iter := Int_List.Next(iter);
	end loop;
	
	put(file, "Hash table filled?: ");
	if(pRecord.NumInsertions >= Long_Long_Integer(pRecord.SizeOfHashTable)) then
	    put(file, "yes");
	else
	    put(file, "no");
	end if;
	new_line(file,2);

    end WritePerformanceRecord;
    
    function NewPerformanceRecord(typeOfProbing : ProbingMethod;
				  sizeOfHashTable : Integer) return PerformanceRecord
    is
	pRecord : PerformanceRecord;	
    begin
	pRecord := new PerformanceRecord_Type;
	pRecord.TypeOfProbing := typeOfProbing;
	pRecord.SizeOfHashTable := sizeOfHashTable;
	return pRecord;
    end NewPerformanceRecord;
    
end Hashing.Performance;
