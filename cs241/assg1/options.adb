with Ada.Text_IO;	use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

package body Options is
	
procedure DisplayOptions 
is
begin
		new_line;
		put_line("Options:");
		put_line(":  " & 
				 Integer'Image(OptionName'Pos(SelectionSort)) & 
				 ". Sort using Selection Sort");
		put_line(":  " & 
				 Integer'Image(OptionName'Pos(InsertionSort)) & 
				 ". Sort using Insertion Sort");
		put_line(":  " & 
				 Integer'Image(OptionName'Pos(ShannonSort)) & 
				 ". Sort using Shannon Sort");
		put_line(":  " & 
				 Integer'Image(OptionName'Pos(ShellSort)) & 
				 ". Sort using Shell Sort");
		put_line(":  " & 
				 Integer'Image(OptionName'Pos(GenerateDiagnostics)) & 
				 ". Generate Diagnostics of included files");
		put_line(":  " & 
				 Integer'Image(OptionName'Pos(Quit)) & 
				 ". Quit the program");
end DisplayOptions;

function GetOption return OptionName
is
	optionNum : Natural;
	optionVal : OptionName;
begin
	put(">  ");
	Try:
	begin
		get(optionNum);
		skip_line;
		optionVal := OptionName'Val(optionNum);
		return optionVal;
	exception
		-- Catch-All
		when others =>
			return InvalidOption;
	end Try;
end GetOption;


end Options;
