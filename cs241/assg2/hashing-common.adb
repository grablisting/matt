with Hashing; use Hashing;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Hashing.Common is

    function H(key : Integer; size : Natural) return Integer
    is
    begin
	return (key mod size);
    end H;

    function LinearProbe(key : Integer; tries : Natural; size : Natural) return Integer
    is
    begin
	return ((key + tries) mod size) + 1;
    end LinearProbe;

    function QuadraticProbe(key : Integer; tries : Natural; size : Natural) return Integer
    is
    begin
	return ((key + tries**2) mod size) + 1;
    end QuadraticProbe;

    function CubicProbe(key : Integer; tries : Natural; size : Natural) return Integer
    is
    begin
	return ((key + tries**3) mod size) + 1;
    end CubicProbe;

    function BeatyProbe(key : Integer; tries : Natural; size : Natural) return Integer
    is
    begin
	return ((key + tries)**2 mod size) + 1;
    end BeatyProbe;

    procedure RunPerformanceTest(table : in out Linear_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord)
    is
	toInsert : Natural;
	probes : Natural;
	loadFactorTarget : Float;
    begin
        pRecord := NewPerformanceRecord(Linear_Hash_Table.ProbeType, Linear_Hash_Table.Size(table));
	loadFactorTarget := pRecord.LoadFactorGap;
	Reset(file);
	
	while( (not Linear_Hash_Table.IsFull(table)) and (not end_of_file(file))) loop
	    get(file, toInsert);
	    probes := Linear_Hash_Table.Insert(table, toInsert);
	    AddProbes(pRecord, probes);
	    AddInsertion(pRecord);
	    if(Linear_Hash_Table.LoadFactor(table) >= loadFactorTarget) then
		loadFactorTarget := loadFactorTarget + pRecord.LoadFactorGap;
		AddLoadFactorStat(pRecord, probes);
	    end if;
	end loop;
		
    end RunPerformanceTest;

    procedure RunPerformanceTest(table : in out Quadratic_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord)
    is
	toInsert : Natural;
	probes : Natural;
	loadFactorTarget : Float;
    begin
        pRecord := NewPerformanceRecord(Quadratic_Hash_Table.ProbeType, Quadratic_Hash_Table.Size(table));
	loadFactorTarget := pRecord.LoadFactorGap;
	Reset(file);
	
	while( (not Quadratic_Hash_Table.IsFull(table)) and (not end_of_file(file))) loop
	    get(file, toInsert);
	    probes := Quadratic_Hash_Table.Insert(table, toInsert);
	    AddProbes(pRecord, probes);
	    AddInsertion(pRecord);
	    if(Quadratic_Hash_Table.LoadFactor(table) >= loadFactorTarget) then
		loadFactorTarget := loadFactorTarget + pRecord.LoadFactorGap;
		AddLoadFactorStat(pRecord, probes);
	    end if;
	end loop;
		
    end RunPerformanceTest;

    procedure RunPerformanceTest(table : in out Cubic_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord)
    is
	toInsert : Natural;
	probes : Natural;
	loadFactorTarget : Float;
    begin
        pRecord := NewPerformanceRecord(Cubic_Hash_Table.ProbeType, Cubic_Hash_Table.Size(table));
	loadFactorTarget := pRecord.LoadFactorGap;
	Reset(file);
	
	while( (not Cubic_Hash_Table.IsFull(table)) and (not end_of_file(file))) loop
	    get(file, toInsert);
	    probes := Cubic_Hash_Table.Insert(table, toInsert);
	    AddProbes(pRecord, probes);
	    AddInsertion(pRecord);
	    if(Cubic_Hash_Table.LoadFactor(table) >= loadFactorTarget) then
		loadFactorTarget := loadFactorTarget + pRecord.LoadFactorGap;
		AddLoadFactorStat(pRecord, probes);
	    end if;
	end loop;
		
    end RunPerformanceTest;

    procedure RunPerformanceTest(table : in out Beaty_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord)
    is
	toInsert : Natural;
	probes : Natural;
	loadFactorTarget : Float;
    begin
        pRecord := NewPerformanceRecord(Beaty_Hash_Table.ProbeType, Beaty_Hash_Table.Size(table));
	loadFactorTarget := pRecord.LoadFactorGap;
	Reset(file);
	
	while( (not Beaty_Hash_Table.IsFull(table)) and (not end_of_file(file))) loop
	    get(file, toInsert);
	    probes := Beaty_Hash_Table.Insert(table, toInsert);
	    AddProbes(pRecord, probes);
	    AddInsertion(pRecord);
	    if(Beaty_Hash_Table.LoadFactor(table) >= loadFactorTarget) then
		put_line("inserted some");
		loadFactorTarget := loadFactorTarget + pRecord.LoadFactorGap;
		AddLoadFactorStat(pRecord, probes);
	    end if;
	end loop;
		
    end RunPerformanceTest;


end Hashing.Common;
