with Ada.Text_IO; use Ada.Text_IO;
with Generic_Linked_list;

package Hashing is
    type PerformanceRecord_Type is private;
    type PerformanceRecord is access PerformanceRecord_Type;
    type ProbingMethod is (LinearProbing, QuadraticProbing, CubicProbing, BeatyProbing, Unknown);

    
    procedure AddProbes(pRecord : in out PerformanceRecord; probes : in Natural);
    procedure AddInsertion(pRecord : in out PerformanceRecord);
    procedure AddLoadFactorStat(pRecord : in out PerformanceRecord; probes : in Natural);

    procedure DisplayPerformanceRecord(pRecord : in out PerformanceRecord);
    procedure WritePerformanceRecord(pRecord : in out PerformanceRecord; file : in out File_Type);
    procedure Free(pRecord : in out PerformanceRecord);

    function GetClosestPrime(size : Natural) return Natural;

private

    package Int_List is new Generic_Linked_List(Integer);

    type PerformanceRecord_Type is 
	record
	    TypeOfProbing : ProbingMethod := Unknown;
	    SizeOfHashTable : Integer := 0;
	    NumProbes : Long_Long_Integer := 0;
	    NumInsertions : Long_Long_Integer := 0;
	    ProbesPerLoadFactor : Int_List.listPtr := null;
	    LoadFactorGap : Float := 0.2;
	end record;
    
    function NewPerformanceRecord(typeOfProbing : ProbingMethod;
				  sizeOfHashTable : Integer) return PerformanceRecord;
   
end Hashing;
