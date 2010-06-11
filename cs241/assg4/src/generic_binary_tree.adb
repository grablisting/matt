with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Unchecked_Deallocation;

package body Generic_Binary_Tree is

	procedure Free is new Ada.Unchecked_Deallocation(Tree_Node, Tree);
	procedure FreeInfo is new Ada.Unchecked_Deallocation(Tree_Info_Type, Tree_Info); 
	procedure Free (I : in out Tree_Info)
	is
	begin
		FreeInfo(I);
	end Free;

	function NewTree(V : Item) return Tree is
		T : Tree;
	begin
		T := new Tree_Node;
		T.Stored := V;
		T.Left_SubTree := null;
		T.Right_SubTree := null;
		return T;
	end NewTree;

   -- Inserts a value into a named binary tree
   procedure Insert (
         T : in out Tree;
         V : in     Item) is
   begin
		if( T = null ) then
			T := NewTree(V);
		elsif ( V < T.Stored ) then
			if ( T.Left_SubTree /= null ) then
				Insert( T.Left_SubTree, V );
			else
				T.Left_SubTree := NewTree(V);
			end if;
		elsif ( V > T.Stored) then
			if ( T.Right_SubTree /= null ) then
				Insert( T.Right_SubTree, V);
			else
				T.Right_SubTree := NewTree(V);
			end if;
		end if;
   end Insert ;

   -- Deletes a value into a named binary tree
	procedure Delete (
		T : in out Tree;
		V : in     Item) 
	is
		node, parent, succ, p_succ : Tree;
	begin
		-- find the node and make sure we actually have to delete something
		node := T;
		parent := T;
		while( node /= null and then node.Stored /= V ) loop
			parent := node;
			if ( V < Node.Stored ) then
				node := node.Left_SubTree;
			else
				node := node.Right_SubTree;
			end if;
		end loop;

		-- if we found it, delete it
		if( node /= null ) then
		
			-- case 1: node has no right child
			if ( node.Right_SubTree = null ) then
				if (node = parent.Right_SubTree) then
					parent.Right_SubTree := node.Left_SubTree;
				elsif (node = parent.Left_SubTree) then
					parent.Left_SubTree := node.Left_SubTree;
				elsif (node = parent) then
					T := node.Left_SubTree;
				end if;
				Free(node);

			-- case 2: node has a right child (with no left child)
			elsif ( node.Right_SubTree.Left_SubTree = null ) then
				node.Right_SubTree.Left_SubTree := node.Left_SubTree;
				if ( node = parent.Right_SubTree ) then
					parent.Right_SubTree := node.Right_SubTree;
				elsif (node = parent.Left_SubTree) then
					parent.Left_SubTree := node.Right_SubTree;
				elsif( node = parent ) then
					T := node.Right_SubTree;
				end if;
				Free(node);

			-- case 3: node has a right child (with a left child)
			elsif ( node.Right_SubTree.Left_SubTree /= null ) then
				-- detach node's inorder successor
				p_succ := node;
				succ := node.Right_SubTree;
				while ( succ.Left_SubTree /= null ) loop
					p_succ := succ;
					succ := succ.Left_SubTree;
				end loop;

				if (p_succ /= node) then
					p_succ.Left_SubTree := succ.Right_SubTree;
				else
					node.Right_SubTree := succ.Right_SubTree;
				end if;

				node.Stored := succ.Stored;
				Free(succ);
			end if;
		end if;

	end Delete ;

	function NewTreeInfo return Tree_Info
	is
		I : Tree_Info;
	begin
		I := new Tree_Info_Type;
		I.MinDepth := Integer'Last;
		I.MaxDepth := 0;
		I.NumLeaves := 0;
		I.NumNodes := 0;
		I.LBalance := 1.0;
		I.RBalance := 1.0;
		return I;
	end NewTreeInfo;

	procedure Display_Tree_Info (
		I : in Tree_Info)
	is
	begin
		Write_Tree_Info(I, Standard_Output);
	end Display_Tree_Info;

	procedure Write_Tree_Info (
		I : in Tree_Info;
		F : in File_Type)
	is
	begin
		put(F, I.NumNodes, 5);
		put(F, "        ");
		put(F, I.NumLeaves, 5);
		put(F, "        ");
		put(F, I.MinDepth, 5);
		put(F, "        ");
		put(F, I.MaxDepth, 5);
		put(F, "        ");
		put(F, I.MaxDepth - I.MinDepth, 5);
		put(F, "        ");
		put(F, I.LBalance, 3, 3, 0);
		put(F, "        ");
		put(F, I.RBalance, 3, 3, 0);
		put(F, "        ");
		new_line(F);
	end Write_Tree_Info;

	procedure Gather_Info_Helper (
		T: in Tree;
		D : in Natural;
		I : in out Tree_Info )
	is
	begin
		if ( T /= null ) then
			I.NumNodes := I.NumNodes + 1;
			if ( T.Left_Subtree = null and T.Right_SubTree = null ) then
				I.NumLeaves := I.NumLeaves + 1;
				if (D < I.MinDepth) then
					I.MinDepth := D;
				end if;
				if (D > I.MaxDepth) then
					I.MaxDepth := D;
				end if;
			else
				Gather_Info_Helper (T.Left_SubTree, D+1, I);
				Gather_Info_Helper (T.Right_SubTree, D+1, I);
			end if;
		end if;
	end Gather_Info_Helper;

	procedure Gather_Info (
		T : in Tree;
		I : in out Tree_Info )
	is
		L, R : Tree_Info;
	begin
		if ( I /= null ) then
			Free(I);
		end if;
		I := NewTreeInfo;
		L := NewTreeInfo;
		R := NewTreeInfo;

		-- case: single node
		if ( T.Left_SubTree = null and T.Right_SubTree = null) then
			I.NumNodes := 1;
			I.NumLeaves := 1;
			I.MaxDepth := 0;
			I.MinDepth := 0;
			I.LBalance := 1.0;
			I.RBalance := 1.0;
		else
			Gather_Info_Helper(T.Left_SubTree, 0, L);
			Gather_Info_Helper(T.Right_SubTree, 0, R);

			I.NumNodes := 1 + L.NumNodes + R.NumNodes;
			I.NumLeaves := L.NumLeaves + R.NumLeaves;
			I.LBalance := Float(L.NumNodes) / Float(R.NumNodes + L.NumNodes);
			I.RBalance := 1.0 - I.LBalance;

			if (L.MaxDepth > R.MaxDepth) then
				I.MaxDepth := 1 + L.MaxDepth;
			else
				I.MaxDepth := 1 + R.MaxDepth;
			end if;

			if (L.MinDepth < R.MinDepth) then
				I.MinDepth := 1 + L.MinDepth;
			else
				I.MinDepth := 1 + R.MinDepth;
			end if;
		end if;
	end Gather_Info;

   -- performs an InOrder traversal of a named
   -- binary tree
   procedure In_Order_Traverse (
         T : in     Tree) is
   begin
      if ( T /= null ) then
		   In_Order_Traverse ( T.Left_SubTree );
		   Visit ( T.Stored );
		   In_Order_Traverse ( T.Right_SubTree);
	  end if;
   end In_Order_Traverse;

   function ">" ( I1, I2 : Item) return Boolean
   is
   begin
	   return (not (I1 < I2) and (I1 /= I2));
   end ">";

end Generic_Binary_Tree ;

