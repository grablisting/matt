with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Hashing; use Hashing;
with Hashing.Common; use Hashing.Common;
with Hashing.Generic_Hash_Table;

package body Assg2_Helper is

    procedure OpenOrCreate(file : in out File_Type; fileName : in String)
    is
    begin
	if(Is_Open(file)) then
	    Close(file);
	end if;
	begin
	    Open(file, Out_File, fileName);
	exception
	    when others => Create(file, Out_File, fileName);
	end;
    end;

    procedure OpenRobust(file : in out File_Type; msg : in String)
    is
	fileName : String(1..256);
	len : Natural := 0;
    begin
	loop
	    if(Is_Open(file)) then
		Close(file);
	    end if;
	    begin
		put(msg);
		get_line(fileName, len);
		Open(file, In_File, fileName(1..len));
		exit;
	    exception
		when others => put_line("ERROR: Could not open file: " & fileName(1..len));
	    end;
	end loop;
    end;

    procedure GetRobust(item : in out Natural; msg : in String)
    is
    begin
	loop
	    begin
		put(msg);
		get(item);
		exit;
	    exception
		when others => null;
	    end;
	end loop;
    end GetRobust;

    procedure RunHashPerformanceTests(inFile : in out File_Type; outFile : in out File_Type; size : in Natural)
    is
	optimalSize : Natural;
	linearTable : Linear_Hash_Table.Hash_Table;
	quadraticTable : Quadratic_Hash_Table.Hash_Table;
	cubicTable : Cubic_Hash_Table.Hash_Table;
	beatyTable : Beaty_Hash_Table.Hash_Table;
	pRecord : PerformanceRecord;
    begin
	put_line("Calculating optimal hash table size...");
	optimalSize := GetClosestPrime(size);
	put_line("Found size. Using " & Integer'Image(optimalSize) & ".");
	put_line("Starting tests...");
	new_line;

	linearTable := Linear_Hash_Table.NewHashTable(optimalSize);
	RunPerformanceTest(linearTable, inFile, pRecord);
	DisplayPerformanceRecord(pRecord);
	WritePerformanceRecord(pRecord, outFile);
	Free(pRecord);
	new_line;
	put_line("Test complete.");

	quadraticTable := Quadratic_Hash_Table.NewHashTable(optimalSize);
	RunPerformanceTest(quadraticTable, inFile, pRecord);
	DisplayPerformanceRecord(pRecord);
	WritePerformanceRecord(pRecord, outFile);
	Free(pRecord);
	new_line;
	put_line("Test complete.");

	cubicTable := Cubic_Hash_Table.NewHashTable(optimalSize);
	RunPerformanceTest(cubicTable, inFile, pRecord);
	DisplayPerformanceRecord(pRecord);
	WritePerformanceRecord(pRecord, outFile);
	Free(pRecord);
	new_line;
	put_line("Test complete.");

	beatyTable := Beaty_Hash_Table.NewHashTable(optimalSize);
	RunPerformanceTest(beatyTable, inFile, pRecord);
	DisplayPerformanceRecord(pRecord);
	WritePerformanceRecord(pRecord, outFile);
	Free(pRecord);
	new_line;
	put_line("Test complete.");
	
    end RunHashPerformanceTests;


end Assg2_Helper;
