with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;

package body Hashing.Generic_Hash_Table is

   function NewHashTable(size : Natural) return Hash_Table
   is
        table : Hash_Table;
   begin
	table := new Hash_Table_Type;
	table.Slots := new Slot_Array_Type(1..size);
	table.Size := size;
	table.Filled := 0;
	return table;
   end NewHashTable;

   procedure Insert(table : in out Hash_Table; key : in Key_Type)
   is
       tries : Natural := 0;
       startIndex : Natural := H(key, table.Size);
       tryIndex : Natural;
   begin
	while(tries < table.Size) loop
	    tryIndex := Probe(startIndex, tries, table.Size);
	    tries := tries + 1;
	    if(table.Slots(tryIndex).Filled = false) then
		table.Slots(tryIndex).Filled := true;
		table.Slots(tryIndex).Key := key;
		table.Filled := table.Filled + 1;
		exit;
	    elsif(table.Slots(tryIndex).Key = key) then
		exit;
	    end if;
	end loop;
   end Insert;

   procedure Delete(table : in out Hash_Table; key : in Key_Type)
   is
      tries : Natural := 0;
      startIndex : Natural := H(key, table.Size);
      tryIndex : Natural;
   begin
	while(tries < table.Size) loop
	    tryIndex := Probe(startIndex, tries, table.Size);
	    tries := tries + 1;
	    if(table.Slots(tryIndex).Filled = false) then
		exit;
	    elsif(table.Slots(tryIndex).Key = key) then
		table.Slots(tryIndex).Filled := false;
		table.Filled := table.Filled - 1;
		exit;
	    end if;
	end loop;
   end Delete;

   function Search(table :Hash_Table; key : in Key_Type) return Boolean
   is
        tries : Natural := 0;
        startIndex : Natural := H(key, table.Size);
        tryIndex : Natural;
   begin
	while(tries < table.Size) loop
	    tryIndex := Probe(startIndex, tries, table.Size);
	    tries := tries + 1;
	    if(table.Slots(tryIndex).Filled = false) then
		return false;
	    elsif(table.Slots(tryIndex).Key = key) then
		return true;
	    end if;
	end loop;
	return false;
   end Search;

   function IsFull(table : Hash_Table) return Boolean
   is
   begin
	return (table.Filled = table.Size);
   end IsFull;

   function Size(table : Hash_Table) return Natural
   is
   begin
	return table.Size;
   end Size;

   procedure DeallocateHashTable is new Ada.Unchecked_Deallocation(Hash_Table_Type, Hash_Table);
   procedure DeallocateSlotArray is new Ada.Unchecked_Deallocation(Slot_Array_Type, Slot_Array);
   procedure Free(table : in out Hash_Table)
   is
   begin
	DeallocateSlotArray(table.Slots);
	DeallocateHashTable(table);
   end Free;

   function LoadFactor(table : Hash_Table) return Float
   is
   begin
	return (Float(table.Filled) / Float(table.Size));
   end LoadFactor;

   function ProbeType return ProbingMethod
   is
   begin
	return typeOfProbing;
   end ProbeType;
   
   function Insert(table : Hash_Table; key : in Key_Type) return Integer
   is
       tries : Natural := 0;
       startIndex : Natural := H(key, table.Size);
       tryIndex : Natural;
   begin
	while(tries < table.Size) loop
	    tryIndex := Probe(startIndex, tries, table.Size);
	    tries := tries + 1;
	    if(table.Slots(tryIndex).Filled = false) then
		table.Slots(tryIndex).Filled := true;
		table.Slots(tryIndex).Key := key;
		table.Filled := table.Filled + 1;
		exit;
	    elsif(table.Slots(tryIndex).Key = key) then
		exit;
	    end if;
	end loop;
	return tries;
   end Insert;

       
end Hashing.Generic_Hash_Table;
