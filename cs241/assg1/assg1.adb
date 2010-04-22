-- Matt Forbes
-- CS241 Assignment 1
-- Sorting Performance

with Ada.Text_IO;   use Ada.Text_IO;
with Assg1_Helper;  use Assg1_Helper;
with Options;	    use Options;

procedure Assg1 is
    option : OptionName;
begin
    put_line("CS241 - Assignment 1");
    put_line("This program will sort a file of integers, and will create 2 new files. One for " &
			 "the now-sorted integers, and one the performance of the sorting algorithm");	

    loop
		DisplayOptions;
		option := GetOption;
	exit when option = Quit;
		HandleOption(option);
    end loop;

end Assg1;
