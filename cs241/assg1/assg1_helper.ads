with Ada.Text_IO; use Ada.Text_IO;
with Options;	use Options;
package Assg1_Helper is

	type FilesArray is Array(Integer range <>) of File_Type;
    
    procedure HandleOption(option : in OptionName);
	procedure OpenSortFiles(unsortedFile : in out File_Type;
							sortedFile : in out File_Type;
							infoFile : in out File_Type);
	procedure CloseSortFiles(unsortedFile : in out File_Type; 
							 sortedFile : in out File_Type; 
							 infoFile : in out File_Type);

	procedure OpenSortFiles(unsortedFile : in out File_Type;
							sortedFile : in out File_Type;
							infoFile : in out File_Type;
							gapFile : in out File_Type);
	procedure CloseSortFiles(unsortedFile : in out File_Type; 
							 sortedFile : in out File_Type; 
							 infoFile : in out File_Type;
							 gapFile : in out File_Type);


end Assg1_Helper;
