with Ada.Text_Io;
with Trees;
procedure T_Trees is

  package My_Tree is new Trees.Tree (Natural);

  T : My_Tree.Tree_Type;

  Spaces : constant String (1 .. 132) := (others => ' ');
  function Image (Elt : in Natural;
                  Level : in Natural) return String is
  begin
    return Spaces (1 .. Level) & Elt'Img;
  end Image;

  -- Dumps the full tree. Sets Saved_Position to current
  procedure  Dump_Tree (T : in out My_Tree.Tree_Type) is
  begin
    My_Tree.Save_Position (T);
    My_Tree.Move_Root (T);
    My_Tree.Dump (T, Image'Access,
                  Ada.Text_Io.Standard_Output, Elder => False);
    Ada.Text_Io.New_Line;
    My_Tree.Move_Saved (T);
  end Dump_Tree;

begin

  -- Add root: 1
  Ada.Text_Io.Put_Line ("Inserting 1 as root");
  My_Tree.Insert_Father (T, 1);

  -- Add 5 children
  Ada.Text_Io.Put_Line ("Inserting 11 to 15 as root children");
  for I in 11 .. 15 loop
    My_Tree.Insert_Child (T, I);
    My_Tree.Move_Father (T);
  end loop;

  Ada.Text_Io.Put_Line (
         "CHECK that 1 has got 5 children:"
       & Natural'Image (My_Tree.Read(T))
       & " has got"
       & Trees.Child_Range'Image (My_Tree.Children_Number(T))
       & " children");

  My_Tree.Move_Child (T);

  -- Add children to children... on branch 15
  Ada.Text_Io.Put_Line ("Adding 151, 1511 and 15111 as 15 family");
  My_Tree.Insert_Child (T, 151);
  My_Tree.Insert_Child (T, 15111);
  My_Tree.Insert_Father (T, 1511);
  Dump_Tree (T);

  -- Insert 0 as root and 2 as brother of 1
  Ada.Text_Io.Put_Line ("Inserting 0 as root and 2 as elder of 1");
  My_Tree.Move_Root (T);
  My_Tree.Save_Position (T);
  My_Tree.Insert_Father (T, 0);
  My_Tree.Move_Saved (T);
  My_Tree.Insert_Brother (T, 2);
  Dump_Tree (T);

  -- Move to 1, replace 14 by 41 and remove 13
  Ada.Text_Io.Put_Line ("Replacing 14 by 41 and removing 13");
  My_Tree.Move_Brother (T, False);
  My_Tree.Move_Child (T);
  My_Tree.Move_Brother (T, False);
  My_Tree.Replace (T, 41);
  My_Tree.Move_Brother (T, False);
  My_Tree.Delete_Current (T);
  Dump_Tree (T);

  -- Swap 2 and 15 tree
  Ada.Text_Io.Put_Line ("Swapping 2 and 15");
  My_Tree.Move_Root (T);
  My_Tree.Move_Child (T, True);
  My_Tree.Save_Position (T);
  My_Tree.Move_Brother (T, False);
  My_Tree.Move_Child (T);
  My_Tree.Swap_Saved (T);
  Dump_Tree (T);

  -- Clear second branch (15)
  Ada.Text_Io.Put_Line ("Clearing branch 15");
  My_Tree.Move_Root (T);
  My_Tree.Move_Child (T, True);
  My_Tree.Delete_Tree (T, False);
  My_Tree.Move_Root (T);
  My_Tree.Dump (T, Image'Unrestricted_Access,
                Ada.Text_Io.Standard_Output, Elder => False);

  -- Clear all
  Ada.Text_Io.Put_Line ("Clearing all");
  My_Tree.Move_Root (T);
  My_Tree.Delete_Tree (T, True);
  begin
    My_Tree.Move_Root (T);
    raise Program_Error;
  exception
    when Trees.No_Cell =>
      Ada.Text_Io.Put_Line ("Empty");
  end;

end T_Trees;

