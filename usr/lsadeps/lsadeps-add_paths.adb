with Dynamic_List, Recurs;
separate (Lsadeps)
procedure Add_Paths is
  -- The index (5 or 9), occurence and position of -I and -R arguments
  --  in order of occurence and index, then sorted by position
  type Arg_Rec is record
    Index : Argument_Parser.The_Keys_Range;
    Occurence : Positive;
    Position : Positive;
    Recursive : Boolean;
    Path : As.U.Asu_Us;
  end record;
  Arg : Arg_Rec;
  package Arg_Dyn_List_Mng is new Dynamic_List (Arg_Rec);
  package Arg_List_Mng renames Arg_Dyn_List_Mng.Dyn_List;
  Args : Arg_List_Mng.List_Type;
  function Less_Than (E1, E2 : Arg_Rec) return Boolean is
  begin
    return E1.Position < E2.Position;
  end Less_Than;
  procedure Sort_Args is new Arg_List_Mng.Sort (Less_Than);

  -- Store all occurences of index
  procedure Store (Index : in Argument_Parser.The_Keys_Range;
                   Recursive : in Boolean) is
    Path : As.U.Asu_Us;
  begin
    for I in 1 .. Arg_Dscr.Get_Nb_Occurences (Index) loop
      Path := As.U.Tus (Arg_Dscr.Get_Option (Index, I));
      if Path.Is_Null then
        Error ("Missing include dir");
      end if;
      Path := As.U.Tus (Directory.Make_Full_Path (Path.Image));
      Check_Dir (Path.Image);
      -- Add this include to paths
      Args.Insert (Arg_Rec'(Index,
                            I,
                            Arg_Dscr.Get_Position (Index, I),
                            Recursive,
                            Path));
    end loop;
  end Store;

  -- Callback called in each subdir
  function Add_Current return Boolean is
    Dir : constant String := Directory.Get_Current;
  begin
    Sort.Add_Path (As.U.Tus (Dir));
    return True;
  end Add_Current;
  procedure Add_Recurs is new Recurs (Add_Current);

  Moved : Boolean;

begin
  -- Store includes and recursive directives: must not be empty
  Store (5, False);
  Store (9, True);
  if Args.Is_Empty then
    return;
  end if;

  -- Sort by position
  Sort_Args (Args);

  -- Insert path of Includes and recursively those of Recursives
  Args.Rewind;
  loop
    -- Add this include to paths
    Args.Get (Arg, Moved => Moved);
    if not Arg.Recursive then
      Sort.Add_Path (Arg.Path);
    else
      Directory.Change_Current (Arg.Path.Image);
      Add_Recurs (Name_Of_Dir => False,
                  In_Current  => True,
                  First_Level_Only =>  False,
                  Leaves_Only => False,
                  Stop_On_Error => True,
                  Follow_Links => False);
    end if;
    exit when not Moved;
  end loop;
  -- Back to current
  Check_Dir ("");
end Add_Paths;

