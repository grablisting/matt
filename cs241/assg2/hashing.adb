with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

package body Hashing is

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

    procedure DisplayPerformanceRecord(pRecord : in out PerformanceRecord)
    is
	stdout : File_Type := Standard_Output;
    begin
	WritePerformanceRecord(pRecord, stdout);
    end DisplayPerformanceRecord;

    procedure WritePerformanceRecord(pRecord : in out PerformanceRecord; file : in out File_Type)
    is
	iter : Int_List.listPtr;
	currentLoadFactor : Float := 0.0;
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
	    currentLoadFactor := currentLoadFactor + pRecord.LoadFactorGap;
	    put(file, "Number of Probes at Load Factor ");
	    put(file, currentLoadFactor, 0, 2, 0);
	    put(file, ": " & Integer'Image(Int_List.value(iter)));
	    new_line(file);
	    iter := Int_List.Next(iter);
	end loop;
	
	put(file, "Hash table filled?: ");
	if(pRecord.NumInsertions >= Long_Long_Integer(pRecord.SizeOfHashTable)) then
	    put(file, "yes");
	else
	    put(file, "no");
	end if;
	new_line(file);

    end WritePerformanceRecord;

    -- The following definition is from http://coding.derkeiler.com/Archive/General/comp.programming/2004-03/0528.html
    type Prime_Array is array(Positive range <>) of Positive;
    subtype Max_Range is Positive range 2..Positive'Last; 
    function Is_Prime (Primes : Prime_Array; Value : Positive ) 
             return Boolean is 
	package Math is new Ada.Numerics.Generic_Elementary_Functions(Float);
        use Math; 
        Result : Boolean  := True; 
        Max_Test : constant Positive := Positive(sqrt(Float(Value)));
     begin
        for Index in Primes'Range loop
           if (Value mod Primes(Index)) = 0 then
              Result := False;
              exit;
           end if;
           -- exit the loop the prime factor is > sqrt of the value
           exit when Primes(index) > Max_Test;
        end loop;
        return Result;
     end Is_Prime; 

    function GetClosestPrime(size : Natural) return Natural
    is
	prime : Natural;
	primeNums : Prime_Array(1..size);
	count : Natural := 1;
	test : Natural := 3;
    begin
	primeNums(1) := 2;
	while(count < size) loop
	    if(Is_Prime(primeNums(1..count), test)) then
		count := count + 1;
		primeNums(count) := test;
		prime := test;
		exit when prime > size;
	    end if;
	    test := test + 2;
	end loop;
	return prime;
    end GetClosestPrime;


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
    
end Hashing;
