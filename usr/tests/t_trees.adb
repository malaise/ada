with Basic_Proc, Trees;
procedure T_Trees is

  package My_Tree is new Trees.Tree (Natural);

  T, T1 : My_Tree.Tree_Type;

  Spaces : constant String (1 .. 132) := (others => ' ');
  function Put (Elt : in Natural;
                Level : in Natural) return Boolean is
  begin
    Basic_Proc.Put_Line_Output (Spaces (1 .. Level*3) & Elt'Img);
    return True;
  end Put;

  -- Dumps the full tree. Sets Saved_Position to current
  procedure  Dump_Tree (T : in out My_Tree.Tree_Type) is
  begin
    My_Tree.Save_Position (T);
    My_Tree.Move_Root (T);
    My_Tree.Iterate (T, Put'Access, Elder => False);
    Basic_Proc.New_Line_Output;
    My_Tree.Restore_Position (T);
  end Dump_Tree;

begin

  -- Add 1 as root then add 5 children 11 .. 15
  Basic_Proc.Put_Line_Output ("Inserting 1 as root");
  Basic_Proc.Put_Line_Output ("Inserting 11 to 15 as root children");
  My_Tree.Insert_Father (T, 1);

  for I in 11 .. 15 loop
    My_Tree.Insert_Child (T, I);
    My_Tree.Move_Father (T);
  end loop;
  Dump_Tree (T);

  -- Check 5 children
  Basic_Proc.Put_Line_Output (
         "Checking that 1 has got 5 children:"
       & Natural'Image (My_Tree.Read(T))
       & " has got"
       & Trees.Child_Range'Image (My_Tree.Children_Number(T))
       & " children");


  -- Add children to children... on branch 15
  Basic_Proc.Put_Line_Output ("Adding 151, 1511 and 15111 as 15 descendents");
  My_Tree.Move_Child (T);
  My_Tree.Insert_Child (T, 151);
  My_Tree.Insert_Child (T, 15111);
  My_Tree.Insert_Father (T, 1511);
  Dump_Tree (T);

  -- Insert 0 as root and 2 as brother of 1
  Basic_Proc.Put_Line_Output ("Inserting 0 as root and 2 as elder of 1");
  My_Tree.Move_Root (T);
  My_Tree.Save_Position (T);
  My_Tree.Insert_Father (T, 0);
  My_Tree.Restore_Position (T);
  My_Tree.Insert_Brother (T, 2);
  Dump_Tree (T);

  -- Move to 1, replace 14 by 41 and remove 13
  Basic_Proc.Put_Line_Output ("Replacing 14 by 41 and removing 13");
  My_Tree.Move_Brother (T, False);
  My_Tree.Move_Child (T);
  My_Tree.Move_Brother (T, False);
  My_Tree.Replace (T, 41);
  My_Tree.Move_Brother (T, False);
  My_Tree.Delete_Current (T);
  Dump_Tree (T);

  -- Swap 2 and 15 tree
  Basic_Proc.Put_Line_Output ("Swapping 2 and 15");
  My_Tree.Move_Root (T);
  My_Tree.Move_Child (T, True);
  My_Tree.Save_Position (T);
  My_Tree.Save_Position (T);
  My_Tree.Move_Brother (T, False);
  My_Tree.Move_Child (T);
  My_Tree.Swap_Saved (T);
  Dump_Tree (T);

  -- Clear second branch (15)
  Basic_Proc.Put_Line_Output ("Addind 21 and deleting branch 15");
  My_Tree.Restore_Position (T);
  My_Tree.Insert_Child (T, 21);
  My_Tree.Move_Root (T);
  My_Tree.Move_Child (T, True);
  My_Tree.Delete_Tree (T, False);
  Dump_Tree (T);

  -- Copy branch 2 below 12
  Basic_Proc.Put_Line_Output ("Copying branch 2 below 12");
  My_Tree.Move_Root (T);
  My_Tree.Move_Child (T, True);
  My_Tree.Move_Child (T, True);
  My_Tree.Save_Position (T);
  My_Tree.Move_Brother (T, False);
  My_Tree.Move_Brother (T, False);
  My_Tree.Copy_Saved (T, True, True);
  Dump_Tree (T);

  -- Delete branch 2
  Basic_Proc.Put_Line_Output ("Deleting branch 2");
  My_Tree.Move_Father (T);
  My_Tree.Move_Brother (T, True);
  My_Tree.Move_Brother (T, True);
  My_Tree.Delete_Tree (T, False);
  Dump_Tree (T);

  -- Copy branch 12 as new
  Basic_Proc.Put_Line_Output ("Copying branch 12");
  My_Tree.Move_Child (T, False);
  My_Tree.Move_Brother (T, True);
  My_Tree.Copy_Tree (T1, T, True, True);
  Basic_Proc.Put_Line_Output ("T");
  Dump_Tree (T);
  Basic_Proc.Put_Line_Output ("T'");
  Dump_Tree (T1);

  -- Swap trees
  Basic_Proc.Put_Line_Output ("Swapping 1 and 2 from trees");
  My_Tree.Move_Root (T);
  My_Tree.Move_Child (T, False);
  My_Tree.Move_Root (T1);
  My_Tree.Move_Child (T1, False);
  My_Tree.Swap_Trees (T, T1);
  Basic_Proc.Put_Line_Output ("T");
  Dump_Tree (T);
  Basic_Proc.Put_Line_Output ("T'");
  Dump_Tree (T1);

  -- Clear all
  Basic_Proc.Put_Line_Output ("Clearing all");
  My_Tree.Move_Root (T);
  My_Tree.Delete_Tree (T, True);
  begin
    My_Tree.Move_Root (T);
    Basic_Proc.Put_Line_Output ("ERROR: Should have raised Trees.No_Cell");
    raise Program_Error;
  exception
    when Trees.No_Cell =>
      Basic_Proc.Put_Line_Output ("Empty OK.");
  end;

end T_Trees;

