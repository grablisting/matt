with Hashing; use Hashing;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Hashing.Common is

    function H(key : Integer; size : Natural) return Integer
    is
    begin
	return key;
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
	loadFactorTarget : Integer;
	avg : Integer;
	section : Integer;
    begin
        pRecord := NewPerformanceRecord(Linear_Hash_Table.ProbeType, Linear_Hash_Table.Size(table));
	loadFactorTarget := 0;
	avg := 0;
	section := 0;
	Reset(file);
	
	while( (not Linear_Hash_Table.IsFull(table)) and (not end_of_file(file))) loop
	    get(file, toInsert);
	    begin
		probes := Linear_Hash_Table.Insert(table, toInsert);
		if(probes < Linear_Hash_Table.Size(table)) then
		    AddProbes(pRecord, probes);
		    AddInsertion(pRecord);
		    avg := avg + probes;
		    section := section + 1;
		else
		    exit;
		end if;
		if(Linear_Hash_Table.LoadFactor(table) >= loadFactorTarget) then
		    AddLoadFactorStat(pRecord, avg / section);
		    avg := 0;
		    section := 0;
		    loadFactorTarget := loadFactorTarget + LoadFactorGap(pRecord);
		end if;
	    exception
		when Duplicate_Exception => null;
	    end;
	end loop;
    end RunPerformanceTest;

    procedure RunPerformanceTest(table : in out Quadratic_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord)
    is
	toInsert : Natural;
	probes : Natural;
	loadFactorTarget : Integer;
	avg : Integer;
	section : Integer;
    begin
        pRecord := NewPerformanceRecord(Quadratic_Hash_Table.ProbeType, Quadratic_Hash_Table.Size(table));
	loadFactorTarget := 0;
	avg := 0;
	section  := 0;
	Reset(file);
	
	while( (not Quadratic_Hash_Table.IsFull(table)) and (not end_of_file(file))) loop
	    get(file, toInsert);
	    begin
		probes := Quadratic_Hash_Table.Insert(table, toInsert);
		if(probes < Quadratic_Hash_Table.Size(table)) then
		    AddInsertion(pRecord);
		    AddProbes(pRecord, probes);
		    avg := avg + probes;
		    section := section + 1;
		else
		    exit;
		end if;
		if(Quadratic_Hash_Table.LoadFactor(table) >= loadFactorTarget) then
		    AddLoadFactorStat(pRecord, avg / section);
		    avg := 0;
		    section := 0;
		    loadFactorTarget := loadFactorTarget + LoadFactorGap(pRecord);
		end if;
	    exception
		when Duplicate_Exception => null;
	    end;
	end loop;
    end RunPerformanceTest;

    procedure RunPerformanceTest(table : in out Cubic_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord)
    is
	toInsert : Natural;
	probes : Natural;
	loadFactorTarget : Integer;
	avg : Integer;
	section : Integer;
    begin
        pRecord := NewPerformanceRecord(Cubic_Hash_Table.ProbeType, Cubic_Hash_Table.Size(table));
	loadFactorTarget := 0;
	avg := 0;
	section := 0;
	Reset(file);
	
	while( (not Cubic_Hash_Table.IsFull(table)) and (not end_of_file(file))) loop
	    get(file, toInsert);
	    begin
		probes := Cubic_Hash_Table.Insert(table, toInsert);
		if(probes < Cubic_Hash_Table.Size(table)) then
		    AddInsertion(pRecord);
		    avg := avg + probes;
		    section := section + 1;
		    AddProbes(pRecord, probes);
		else
		    exit;
		end if;
		if(Cubic_Hash_Table.LoadFactor(table) >= loadFactorTarget) then
		    AddLoadFactorStat(pRecord, avg / section);
		    avg := 0;
		    section := 0;
		    loadFactorTarget := loadFactorTarget + LoadFactorGap(pRecord);
		end if;
	    exception
		when Duplicate_Exception => null;
	    end;
	end loop;
    end RunPerformanceTest;

    procedure RunPerformanceTest(table : in out Beaty_Hash_Table.Hash_Table;
				file : in out File_Type; 
				pRecord : out PerformanceRecord)
    is
	toInsert : Natural;
	probes : Natural;
	loadFactorTarget : Integer;
	avg : Integer;
	section : Integer;
    begin
        pRecord := NewPerformanceRecord(Beaty_Hash_Table.ProbeType, Beaty_Hash_Table.Size(table));
	loadFactorTarget := 0;
	avg := 0;
	section := 0;
	Reset(file);
	
	while( (not Beaty_Hash_Table.IsFull(table)) and (not end_of_file(file))) loop
	    get(file, toInsert);
	    begin
		probes := Beaty_Hash_Table.Insert(table, toInsert);
		if(probes < Beaty_Hash_Table.Size(table)) then
		    AddInsertion(pRecord);
		    avg := avg + probes;
		    section := section + 1;
		    AddProbes(pRecord, probes);
		else
		    exit;
		end if;
		if(Beaty_Hash_Table.LoadFactor(table) >= loadFactorTarget) then
		    AddLoadFactorStat(pRecord, avg / section);
		    avg := 0;
		    section := 0;
		    loadFactorTarget := loadFactorTarget + LoadFactorGap(pRecord);
		end if;
	    exception
		when Duplicate_Exception => null;
	    end;
	end loop;
		
    end RunPerformanceTest;


end Hashing.Common;
