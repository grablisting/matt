generic
    type Key_Type is private;
    typeOfProbing : ProbingMethod;
    with function H(key : Key_Type; size : Integer) return Integer;
    with function Probe(index : Integer; tries : Natural; size : Natural) return Integer;
package Hashing.Generic_Hash_Table is
    
   type Hash_Table_Type is private;
   type Hash_Table is access Hash_Table_Type;

   function NewHashTable(size : Natural) return Hash_Table;
   procedure Insert(table : in out Hash_Table; key : in Key_Type);
   function Insert(table : Hash_Table; key : in Key_Type) return Integer;
   procedure Delete(table : in out Hash_Table; key : in Key_Type);
   function Search(table : Hash_Table; key : in Key_Type) return Boolean;
   function IsFull(table : Hash_Table) return Boolean;
   function Size(table : Hash_Table) return Natural;
   procedure Free(table : in out Hash_Table);

   function LoadFactor(table : Hash_Table) return Integer;
   function ProbeType return ProbingMethod;
   
private

    type Slot is
	record
	    Key : Key_Type;
	    Filled : Boolean := false;
	end record;

    type Slot_Array_Type is Array(Integer range <>) of Slot;
    type Slot_Array is access Slot_Array_Type;

    type Hash_Table_Type is 
	record
	    Slots : Slot_Array;
	    Size : Natural;
	    Filled : Natural;
	end record;
    
end Hashing.Generic_Hash_Table;
