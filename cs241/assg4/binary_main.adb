with Ada.Text_IO ;
use Ada.Text_IO ;
with Ada.Integer_Text_IO ;
use Ada.Integer_Text_IO ;
with Generic_Binary_Tree ;
procedure Binary_Main is

   procedure My_Visit (
         V : in     Integer) is
   begin
      Ada.Integer_Text_IO.Put (Item => V) ;
      Ada.Text_IO.New_Line ;
   end My_Visit ;

   package Integer_Binary_Tree is new Generic_Binary_Tree
      (
      Item    => Integer,
      "<" => "<",
      Visit   => My_Visit) ;

   F : File_Type;
   OutFile : File_Type;
   -- Used to hold the value read from the input file:
   Value : Integer;
   -- Used to hold the length of the input filename:
   Length : Natural;
   Name   : String (1 .. 25);
   T      : Integer_Binary_Tree.Tree;
   I	  : Integer_Binary_Tree.Tree_Info;

   OutFileName : constant String := "out.dat";


begin

   Put (Item => "Input Filename:") ;
   Get_Line (
      Item => Name,
      Last => Length) ;
   Open (
      File => F,
      Mode => In_File,
      Name => Name (1 .. Length)) ;
	
	begin
		Open(OutFile,
			 Out_File,
			 OutFileName);
	exception
		when others =>
			Create(OutFile,
				   Out_File,
				   OutFileName);
	end;


   -- Read all values from the input and insert them into the BST
   put(OutFile, "# #nodes     ");
   put(OutFile, "| #leaves    ");
   put(OutFile, "| min depth  ");
   put(OutFile, "| max depth  ");
   put(OutFile, "| depth diff ");
   put(OutFile, "| %left      ");
   put(OutFile, "| %right     "); 
   new_line(OutFile);
   while not End_Of_File (File => F) loop
      Get (
         File => F,
         Item => Value) ;
      Integer_Binary_Tree.Insert (
         T => T,
         V => Value) ;
	  Integer_Binary_Tree.Gather_Info (T => T, I => I) ;
      Integer_Binary_Tree.Write_Tree_Info (I => I, F => OutFile);
   end loop ;

   -- Do a preorder traverse of the
   Integer_Binary_Tree.In_Order_Traverse (T => T) ;
   Integer_Binary_Tree.Display_Tree_Info (I => I);
   Integer_Binary_Tree.Free (I => I);

   Reset (File => F) ;

   while not End_Of_File (File => F) loop
      Get (
         File => F,
         Item => Value) ;

      put_line("Deleting " & Integer'Image(Value) & "..................");
      Integer_Binary_Tree.Delete (
         V => Value,
         T => T) ;
      --Integer_Binary_Tree.In_Order_Traverse (T => T) ;
	  --new_line;
   end loop ;

end Binary_Main ;

