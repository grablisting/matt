with Ada.Text_IO; use Ada.Text_IO;

package body Easy_IO is

    type FileName is
	record
	    Name : String(1..256);
	    Len : Natural;
	end record;

    function GetFileName return FileName 
    is
	name : FileName;
    begin
	get_line(name.Name, name.Len);
	return name;
    end GetFileName;

    function ToString(name : FileName) return String
    is
    begin
	return name.Name(1..name.Len);
    end ToString;
	
    
    function GetInputFile(msg : String) return File_Type
    is
	file : File_Type;
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
	return file;
    end GetInputFile;

    function GetOutputFile(msg : String) return File_Type
    is
	file : File_Type;
	name : FileName;
    begin
	loop
	    begin
		put(msg);
		name := GetFileName;
		Open(file, Out_File, ToString(name));
		exit;
	    exception
		when others =>
		    Create(file, Out_File, ToString(name));
	    end;
	end loop;
    end GetOutputFile;
end Easy_IO;
