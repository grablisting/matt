with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
generic

   type Item is private;

   with procedure Visit (
         I : in     Item);

   with function "<" (
         I1,
         I2 : Item)
     return Boolean;

package Generic_Binary_Tree is

   type Tree is private;
   type Tree_Info is private;

-- Inserts a value into a named binary tree
   procedure Insert (
         T : in out Tree;
         V : in     Item);

 -- Deletes a value into a named binary tree
   procedure Delete (
         T : in out Tree;
         V : in     Item);

   -- performs an InOrder traversal of a named
   -- binary tree
   procedure In_Order_Traverse (
         T : in     Tree);

   procedure Gather_Info (
		 T : in		Tree;
		 I : in out	Tree_Info );

   procedure Display_Tree_Info (
		 I : in Tree_Info );
	
   procedure Write_Tree_Info (
		 I : in Tree_Info;
		 F : in File_Type);

   procedure Free(I : in out Tree_Info);

private

   type Tree_Node is
      record
         Stored        : Item;
         Right_Subtree,
         Left_Subtree  : Tree;
      end record;

   type Tree is access Tree_Node;

   type Tree_Info_Type is
		record
			MinDepth,
			MaxDepth,
			NumLeaves,
			NumNodes : Natural;
			LBalance,
			RBalance : Float;
		end record;
	
    type Tree_Info is access Tree_Info_Type;

	function NewTreeInfo return Tree_Info;

    function ">" (
		I1,
		I2 : Item)
	 return Boolean;

end Generic_Binary_Tree ;

