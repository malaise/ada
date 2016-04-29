with Trees, Unlimited_Pool;

separate (Dtd_Generator.Merge)
package  body Sequences is

  -- Tree of possible solutions
  -- A child of current cell can be this:
  type Cell_Child_Kind_List is (Step_Both, Skip_Cur, Insert_Val, Dead_End);
  type Cell_Type is record
    -- Which kind of child are we
    Kind : Cell_Child_Kind_List := Step_Both;
    -- Current deviation at this cell
    Deviation : Integer := 0;
  end record;
  Def_Cell : constant Cell_Type := (others => <>);
  package Cell_Tree_Mng is new Trees.Tree (Cell_Type);

  -- Dump tree for debug
  function Dump_Cell (Cell : in out Cell_Type;
                      Level : in Natural) return Trees.Iteration_Policy is
    Pad : constant String (1 .. 2 * Level) := (others => ' ');
  begin
    Basic_Proc.Put_Line_Error (Pad & Mixed_Str (Cell.Kind'Img)
                             & " " & Cell.Deviation'Img);
    return Trees.Go_On;
  end Dump_Cell;
  procedure Dump_Tree (T : in out Cell_Tree_Mng.Tree_Type) is
  begin
    Dbg ("Tree dump:");
    if T.Is_Empty then
      return;
    end if;
    T.Save_Position;
    T.Move_Root;
    T.Iterate (Dump_Cell'Unrestricted_Access, False);
    T.Restore_Position;
  end Dump_Tree;

  -- Stack of solutions
  package Sol_Pool is new Unlimited_Pool (Cell_Child_Kind_List);
  package Sol_Poo_Mng renames Sol_Pool.Upool;
  Solution : Sol_Poo_Mng.Pool_Type;

  -- Recusively scan tree (in order because step_both is the
  --  youngest child)
  -- Update the current solution while scanning
  -- Return True when best deviation has been reached
  Deviation : Integer;
  function Build_Solution (T : in out Cell_Tree_Mng.Tree_Type;
                           Child : in Boolean)
           return Boolean is
    Cell : Cell_Type;
  begin
    Cell := T.Read;
    Dbg ("  Curr is "
         & (if Child then " child" else "brother")
         & " with deviation " & Cell.Deviation'Img
         & " and" & Natural'Image (T.Children_Number) & " children");
    if not Child then
      -- A brother
      Solution.Pop;
    end if;
    -- Push new brother ot child
    Solution.Push (Cell.Kind);

    -- Check if this is the end of branch, not dead end,
    --  and with expected "best" deviation
    if T.Children_Number = 0
    and then Cell.Kind /= Dead_End
    and then Cell.Deviation = Deviation then
      -- A leaf with the correct best deviation, keep it
      Dbg ("  Got it OK");
      T.Move_Father;
      return True;
    end if;

    -- Navigate: First child
    if T.Children_Number = 0 then
      Dbg ("  No child child");
    else
      Dbg ("  Move child");
      T.Move_Child (Eldest => False);
      if Build_Solution (T, True) then
        if T.Has_Father then
          Dbg ("  Move father cause OK");
          T.Move_Father;
        end if;
        return True;
      end if;
      Dbg ("  Back from child"
         & " with" & Natural'Image (T.Children_Number) & " children");
    end if;

    -- Move to brother
    while T.Has_Brother loop
      Dbg ("  Move brother");
      T.Move_Brother;
      if Build_Solution (T, False) then
        if T.Has_Father then
          Dbg ("  Move father cause OK");
          T.Move_Father;
        end if;
        return True;
      end if;
      Dbg ("  Back from brother"
         & " with" & Natural'Image (T.Children_Number) & " children");
    end loop;
    Dbg ("  No (more) brother");

    -- Move to father
    if T.Has_Father then
      Dbg ("  Move father");
      Solution.Pop;
      T.Move_Father;
    else
      Dbg ("  Back at root");
    end if;
    return False;
  end Build_Solution;

  -- Increment Into or Val index, return True if it has overflown
  -- Or decrement index if possible
  procedure Step (Incr : in Boolean;
                  I : in out Natural) is
  begin
    if Incr then
      I := I + 1;
    else
      I := I - 1;
    end if;
  end Step;

  -- Merge Val sequence into Into
  procedure Merge_Sequences is
    -- Indexes in Into and Val sequences, 0 when out of bounds
    Vali, Intoi : Positive;
    -- Lengths of sequences
    Intolen : constant Natural := Into.Children.Length;
    Vallen : constant Natural := Val.Children.Length;
    -- The tree of possibilities
    Tree : Cell_Tree_Mng.Tree_Type;
    -- If sequences have same names, we can avoid a merge
    Same_Names : Boolean;
    -- Current child
    Child : Child_Type;
    -- Resulting sequence
    Result : Sol_Poo_Mng.Pool_Type;
    Kind : Cell_Child_Kind_List;

    -- Update the 'best deviation so far'
    procedure Update_Deviation (By : Integer) is
    begin
      if By < Deviation then
        Deviation := By;
      end if;
    end Update_Deviation;

    -- Recursive procedure that scans the solutions and builds the tree
    procedure Explore is
      -- Current Into and Val element
      Curinto, Curval : Child_Type;
      -- The cell being inserted
      Cell, Child : Cell_Type;
      -- Skip negative options when Step_Both is possible
      Stepped : Boolean;
    begin
      -- Read: Tree points to current cell, indexes point to unbouded arrays
      Cell := Tree.Read;

      if Logger.Debug_On then
        if Intoi /= Intolen + 1 then
          Dbg ("  Into is " & Into.Children.Element (Intoi).Name.Image);
        end if;
        if Vali /= Vallen + 1 then
          Dbg ("  Val is " & Val.Children.Element (Vali).Name.Image);
        end if;
        Dbg ("  Devi =" & Cell.Deviation'Img);
      end if;

      -- Read chidren
      if Intoi /= Intolen + 1 then
        Curinto := Into.Children.Element (Intoi);
      end if;
      if Vali /= Vallen + 1 then
        Curval := Val.Children.Element (Vali);
      end if;

      -- Insert solution of stepping both (when they match)
      Stepped := False;
      if Intoi /= Intolen + 1 and then  Vali /= Vallen + 1
      and then Curinto.Name = Curval.Name then
        Child := Def_Cell;
        Child.Kind := Step_Both;
        Child.Deviation := Cell.Deviation + Dev_Step_Both;
        Dbg ("    Add stepping both");
        -- Update current and insert child
        Tree.Insert_Child (Child);
        Step (True, Intoi);
        Step (True, Vali);
        Explore;
        Step (False, Intoi);
        Step (False, Vali);
        Tree.Move_Father;
        Stepped := True;
      end if;

      -- Insert solutions of skipping current Into, as optional
      if Intoi /= Intolen + 1 and then not Stepped then
        Child := Def_Cell;
        Child.Kind := Skip_Cur;
        if Curinto.Opt then
          Child.Deviation := Cell.Deviation + Dev_Skip_Cur_Opt;
        else
          Child.Deviation := Cell.Deviation + Dev_Skip_Cur_Man;
        end if;
        if Check_Deviation (Child.Deviation) then
          -- Update current and insert child
          Dbg ("    Add skipping of Into, Opt " & Mixed_Str (Curinto.Opt'Img));
          Tree.Insert_Child (Child);
          Step (True, Intoi);
          Explore;
          Step (False, Intoi);
          Tree.Move_Father;
        else
          -- Dead end
          Child.Kind := Dead_End;
          Tree.Insert_Child (Child);
          Tree.Move_Father;
        end if;
      end if;

      -- Insert solution of inserting current Val, as optional, before current
      --  Into
      if Vali /= Vallen + 1 and then not Stepped then
        Child := Def_Cell;
        Child.Kind := Insert_Val;
        Child.Deviation := Cell.Deviation + Dev_Insert_Val;
        if Check_Deviation (Child.Deviation) then
          Dbg ("    Add inserting Val " & Curval.Name.Image & " as Opt");
          -- Update current and insert child
          Tree.Insert_Child (Child);
          Step (True, Vali);
          Explore;
          Step (False, Vali);
          Tree.Move_Father;
        else
          -- Dead end
          Child.Kind := Dead_End;
          Tree.Insert_Child (Child);
          Tree.Move_Father;
        end if;
      end if;

      -- When on a leaf (end of exploration of a branch),
      --  update best deviation
      if Intoi = Intolen + 1 and then  Vali = Vallen + 1 then
        Update_Deviation (Cell.Deviation);
      end if;
    end Explore;

  begin
    -- Init to start
    if Logger.Debug_On then
      Dbg ("Merging sequence " &  Sequence_Image (Val.Children));
      Dbg (" into " & Sequence_Image (Into.Children));
    end if;

    -- Optim if lists are similar (same sequence of names)
    -- Length of Val has already been through Check_Elements
    Same_Names := False;
    if Intolen = Vallen then
      Same_Names := True;
      for I in 1 .. Intolen loop
        if Into.Children.Element (I).Name /= Val.Children.Element (I).Name then
          Same_Names := False;
          exit;
        end if;
      end loop;
    end if;
    if Same_Names then
      Dbg("Optim");
      -- Just propagate Mult from Val to Into
      for I in 1 .. Intolen loop
        Child := Into.Children.Element (I);
        if Val.Children.Element (I).Mult and then not Child.Mult then
          Child.Mult := True;
          Into.Children.Replace_Element (I, Child);
        end if;
      end loop;
      -- Done
      return;
    end if;

    -- Initialize the scan, insert a root in the tree
    Vali := 1;
    Intoi := 1;
    Deviation := Integer'Last;
    Tree.Insert_Father (Def_Cell);

    -- Explore the solutions and build the tree of possible combinations
    Explore;
    if Logger.Debug_On then
      Dump_Tree (Tree);
      Dbg ("Best deviation is " & Deviation 'Img);
    end if;

    -- Check best deviation
    if not Check_Deviation (Deviation) then
      Dbg ("  Deviation exceeded => Choice");
      Into.Kind := Choice;
      -- Remove duplicates and merge lists A (does Check_Elements)
      Reduce (Into);
      Reduce (Val);
      Merge_Lists;
      return;
    end if;

    -- Identify the sequence of solutions that leads to the best deviation
    -- Scan the tree, maintain the current solution, give-up when reached
    Solution.Clear;
    Tree.Move_Root;
    if not Build_Solution (Tree, True) then
      -- Should not occur
      Dbg ("  Best deviation not found => Choice");
      Into.Kind := Choice;
      Reduce (Into);
      Reduce (Val);
      Merge_Lists;
      return;
    end if;

    -- Check length of solution
    if not Check_Elements (Result.Length, 0) then
      Dbg ("  Max elements exceed in best solution => Any");
      Into.Kind := Any;
      return;
    end if;

    -- Generate the corresponding sequence
    -- Revert the solution
    while not Solution.Is_Empty loop
      Result.Push (Solution.Pop);
    end loop;
    -- Remove the solution corresponding to the root that we inserted in
    --  the tree
    Result.Pop;
    -- Apply the solution
    Vali := 1;
    Intoi := 1;
    while not Result.Is_Empty loop
      Kind := Result.Pop;
      Dbg ("  Apply " &  Mixed_Str (Kind'Img));
      case Kind is
        when Step_Both =>
          -- Propagate Mult from Val to Into if necessary
          Child := Val.Children.Element (Vali);
          if Child.Mult then
            Child := Into.Children.Element (Intoi);
            if not Child.Mult then
              Child.Mult := True;
              Into.Children.Replace_Element (Intoi, Child);
            end if;
          end if;
          -- Move to next Val and next Into
          Step (True, Intoi);
          Step (True, Vali);
        when Skip_Cur =>
          -- Make current Into Opt if necessary
          Child := Into.Children.Element (Intoi);
          if not Child.Opt then
            Child.Opt := True;
            Into.Children.Replace_Element (Intoi, Child);
          end if;
          -- Move to next Into
          Step (True, Intoi);
        when Insert_Val =>
          Child := Val.Children.Element (Vali);
          Child.Opt := True;
          Into.Children.Insert (Intoi, Child);
          -- Move to next Val and remain in same Into
          Step (True, Vali);
          Step (True, Intoi);
        when Dead_End =>
          -- Should not occur
          Basic_Proc.Put_Line_Error (
              "Unexpected Dead_End cell while applying solution");
          raise Program_Error;
      end case;
    end loop;

    -- Done
    Tree.Delete_Tree;
    if Logger.Debug_On then
      Dbg ("  Merged into " & Sequence_Image (Into.Children));
    end if;

  end Merge_Sequences;

end Sequences;

