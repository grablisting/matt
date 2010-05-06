with Ada.Text_IO; use Ada.Text_IO;
with Assg2_Helper; use Assg2_helper;

procedure Assg2 is
    cmd : StrBuffer;
    cmdLen : Natural := 0;
    quitCmd : Boolean;

    outFileName : constant String := "performance.dat";
    
    size : Natural := 0;
    inFile, outFile : File_Type;

begin
    put_line("CS241 Assignment Two");
    put_line("Running performance tests on different types of hash table probing.");
    put_line("All performance stats generated will be written to file: " & outFileName);
    new_line;
    
    OpenOrCreate(outFile, outFileName);

    loop
	OpenRobust(inFile, "Please enter the filename of data to insert: ");
	GetRobust(size, "Enter the size of the hash table to use: ");

	RunHashPerformanceTests(inFile, outFile, size);
	Close(inFile);

	put_line("Run more performance tests? (y/n)");

	loop
	    get_line(cmd, cmdLen);
	    if(cmd(1..cmdLen) = "y") then
		quitCmd := false;
		exit;
	    elsif(cmd(1..cmdLen) = "n") then
		quitCmd := true;
		exit;
	    else
		put_line("Please enter y or n");
	    end if;
	end loop;
	
	exit when quitCmd;
    end loop;
    
    Close(outFile);
	    
end Assg2;
