-- Matt Forbes
-- IO package body
with Ada.Text_IO; use Ada.Text_IO;

package body Easy_IO is

    -- Type to conveniently store information about a file
    type FileName is
	record
	    Name : String(1..256);
	    Len : Natural;
	end record;

    -- Get a filename from stdin, and return a FileName record of it
    function GetFileName return FileName 
    is
	name : FileName;
    begin
	get_line(name.Name, name.Len);
	return name;
    end GetFileName;

    -- Given a FileName record, return an exact string of it's contents
    function ToString(name : FileName) return String
    is
    begin
	return name.Name(1..name.Len);
    end ToString;
	
    
    -- Robustly get an input file from stdin
    procedure GetInputFile(msg : in String; file : out File_Type)
    is
	name : FileName;
    begin
	loop
	    begin
		put(msg);
		name := GetFileName;
		Open(file, In_File, ToString(name));
		exit;
	    exception
		when others =>
		    skip_line;
		    put_line("Could not open file: " & ToString(name));
	    end;
	end loop;
    end GetInputFile;

    -- Robustly get an output file, if it doesn't exist, create it.
    procedure GetOutputFile(msg : in String; file : out File_Type)
    is
	name : FileName;
    begin
	begin
	    put(msg);
	    name := GetFileName;
	    Open(file, Out_File, ToString(name));
	exception
	    when others =>
		Create(file, Out_File, ToString(name));
	end;
    end GetOutputFile;
end Easy_IO;
