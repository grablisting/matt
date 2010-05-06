with Hashing.Generic_Hash_Table;
package Hashing.Common is

    function H(key : Integer; size : Natural) return Integer; 

    function LinearProbe(key : Integer; tries : Natural; size : Natural) return Integer;
    function QuadraticProbe(key : Integer; tries : Natural; size : Natural) return Integer;
    function CubicProbe(key : Integer; tries : Natural; size : Natural) return Integer;
    function BeatyProbe(key : Integer; tries : Natural; size : Natural) return Integer;

    package Linear_Hash_Table is new Generic_Hash_Table(Key_Type => Natural,
						      TypeOfProbing => LinearProbing,
						      H => H,
						      Probe => LinearProbe);
    

    package Quadratic_Hash_Table is new Generic_Hash_Table(Key_Type => Natural,
						         TypeOfProbing => QuadraticProbing,
						         H => H,
						         Probe => QuadraticProbe);


    package Cubic_Hash_Table is new Generic_Hash_Table(Key_Type => Natural,
						     TypeOfProbing => CubicProbing,
						     H => H,
						     Probe => CubicProbe);


    package Beaty_Hash_Table is new Generic_Hash_Table(Key_Type => Natural,
						     TypeOfProbing => BeatyProbing,
						     H => H,
						     Probe => BeatyProbe);

    
   procedure RunPerformanceTest(table : in out Linear_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord);

   procedure RunPerformanceTest(table : in out Quadratic_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord);

   procedure RunPerformanceTest(table : in out Cubic_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord);

   procedure RunPerformanceTest(table : in out Beaty_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord);

end Hashing.Common;
