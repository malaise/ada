with As.U.Utils, Dynamic_List, Recurs;
separate (Lsadeps)
procedure Add_Paths is
  -- The index (Include or Recursive index), occurence and position of -I
  --  and -R arguments in order of occurence and index, then sorted by position
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
  -- The list of includes
  Includes : Arg_List_Mng.List_Type;
  function Less_Than (E1, E2 : Arg_Rec) return Boolean is
    (E1.Position < E2.Position);
  procedure Sort_Args is new Arg_List_Mng.Sort (Less_Than);

  -- Store all occurences of index in includes
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
      Includes.Insert (Arg_Rec'(Index,
                            I,
                            Arg_Dscr.Get_Position (Index, I),
                            Recursive,
                            Path));
    end loop;
  end Store;

  -- Store all occurences of exclude
  Excludes : As.U.Utils.Asu_Ua.Unb_Array;
  procedure Store_Excludes is
    Path : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    for I in 1 .. Arg_Dscr.Get_Nb_Occurences (Exclude_Index) loop
      Path := As.U.Tus (Arg_Dscr.Get_Option (Exclude_Index, I));
      if Path.Is_Null then
        Error ("Missing exclude dir");
      end if;
      if Directory.Basename (Path.Image) /= Path then
        Error ("Incorrect exclude dir name " & Path.Image);
      end if;
      -- Add this include to paths
      Excludes.Append (Path);
    end loop;
  end Store_Excludes;

  -- Callback called in each subdir
  procedure Add_Current (Path : in String;
                         Result : out Boolean;
                         Go_On  : out Boolean) is
    Dir : constant String := Directory.Get_Current;
  begin
    Result := True;
    -- If basename of Path is excluded then set not Go_On
    if Excludes.Locate (As.U.Tus (Directory.Basename (Path))) /= 0 then
      -- Current dir name matches an exclusion, skip it and its subdirs
      Debug.Logger.Log_Debug ("  Skipping subdir " & Path);
      Go_On := False;
      return;
    end if;
    Debug.Logger.Log_Debug ("  Adding subdir " & Path);
    Sort.Add_Path (As.U.Tus (Dir));
    Go_On := True;
  end Add_Current;

  Moved : Boolean;

begin
  -- Store includes and recursive directives: must not be empty
  Store (Include_Index, False);
  Store (Recursive_Index, True);
  if Includes.Is_Empty then
    return;
  end if;
  -- Store excludes, check they are basic names
  Store_Excludes;

  -- Sort by position
  Sort_Args (Includes);

  -- Insert path of Includes and recursively those of Recursives
  Includes.Rewind;
  loop
    -- Add this include to paths
    Includes.Get (Arg, Moved => Moved);
    if not Arg.Recursive then
      Debug.Logger.Log_Debug ("Adding include dir " & Arg.Path.Image);
      Sort.Add_Path (Arg.Path);
    else
      Directory.Change_Current (Arg.Path.Image);
      Debug.Logger.Log_Debug ("Adding recursive dir " & Arg.Path.Image);
      Recurs (Do_In_Dir        => Add_Current'Access,
              Name_Of_Dir      => False,
              In_Current       => True,
              First_Level_Only => False,
              Leaves_Only      => False,
              Stop_On_Error    => True,
              Follow_Links     => False);
    end if;
    exit when not Moved;
  end loop;

  -- Back to current
  Check_Dir ("");
end Add_Paths;

