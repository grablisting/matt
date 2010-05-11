with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Long_Long_Integer_Text_IO; use Ada.Long_Long_Integer_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Unchecked_Deallocation;

package body Hashing is

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
		exit when prime >= size;
	    end if;
	    test := test + 2;
	end loop;
	return prime;
    end GetClosestPrime;
    
end Hashing;
