with Ada.Sequential_Io;
with Dir_Mng;
with Pers_Mng, Str_Mng, Mesu_Fil;

-- Mesure selection management
package body Mesu_Sel is
  use Afpx;

  Saved_List : Afpx.Line_List_Mng.List_Type;

  List_File_Name : constant String := "SELECTIO.LST";

  package List_Io is new Ada.Sequential_Io (Mesu_Nam.File_Name_Str);
  List_File : List_Io.File_Type;

  procedure Copy_List (From, To : in out Afpx.Line_List_Mng.List_Type) is
  begin
    -- Delete dest list
    To.Delete_List;
    if not From.Is_Empty then
      To.Insert_Copy (From);
      -- Set same pos
      To.Move_At (From.Get_Position);
    end if;
  end Copy_List;

  procedure Save_List is
  begin
    Copy_List (From => Line_List, To => Saved_List);
  end Save_List;

  function Date_Match (Date, After, Before : Mesu_Def.Date_Str)
  return Boolean is
  begin
    if Str_Mng.Is_Spaces (After) and then Str_Mng.Is_Spaces (Before) then
      -- No criteria : date matches
      return True;
    elsif Str_Mng.Is_Spaces (After) then
      -- Only before : Date has to be < before
      return Date < Before;
    elsif Str_Mng.Is_Spaces (Before) then
      -- Only after : date has to be >= after
      return Date >= After;
    elsif After <= Before then
      -- After <= Before : has to be after <= date < before
      return Date >= After and then Date < Before;
    else
      -- After > Before : has to be after >= date or  date < before
      return Date >= After or else Date < Before;
    end if;
  end Date_Match;

  function Same_File (L1, L2 : Line_Rec) return Boolean is
    F1, F2 : Mesu_Nam.File_Name_Str;
  begin
    Str_Mng.Format_List_To_Mesure (L1, F1);
    Str_Mng.Format_List_To_Mesure (L2, F2);
    return F1 = F2;
  end Same_File;

  function Less_Than (L1, L2 : Line_Rec) return Boolean is
    F1, F2 : Mesu_Nam.File_Name_Str;
    D1, D2 : Mesu_Nam.File_Date_Str;
    N1, N2 : Mesu_Nam.File_No_Str;
    P1, P2 : Mesu_Nam.File_Pid_Str;
  begin
    -- Do not sort on file_name cause 2000 (=>00) < 1999 (=>99)
    Str_Mng.Format_List_To_Mesure (L1, F1);
    Mesu_Nam.Split_File_Name (F1, D1, N1, P1);
    Str_Mng.Format_List_To_Mesure (L2, F2);
    Mesu_Nam.Split_File_Name (F2, D2, N2, P2);
    --  Splitting file name gives full year number
    return D1 & N1 & P1 < D2 & N2 & P2;
  end Less_Than;


  function File_Search is new Line_List_Mng.Search (Same_File);
  procedure File_Sort   is new Line_List_Mng.Sort   (Less_Than);


  -- Add records to selection
  procedure Add_Selection (Criteria : in Criteria_Rec) is
    Saved_Pos : Natural;
    Pos       : Positive;
    First_Pers, Last_Pers : Natural;
    The_Files : Dir_Mng.File_List_Mng.List_Type;
    File : Dir_Mng.File_Entry_Rec;
    Person : Pers_Def.Person_Rec;
    File_Name : Mesu_Nam.File_Name_Str;
    Date_S : Mesu_Nam.File_Date_Str;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;
    Ok     : Boolean;
    Mesure : Mesu_Def.Mesure_Rec;
    Line   : Afpx.Line_Rec;
  begin
    if Pers_Def.The_Persons.Is_Empty then
      return;
    end if;

    -- Save current position
    if Line_List.Is_Empty then
      Saved_Pos := 0;
    else
      Saved_Pos := Line_List.Get_Position;
    end if;
    -- Save list
    Save_List;

    -- Set first and last person indexes
    if Str_Mng.Is_Spaces (Criteria.Name) then
      -- No name => all persons
      First_Pers := 1;
      Last_Pers := Pers_Def.The_Persons.List_Length;
    elsif Str_Mng.Is_Spaces (Criteria.Activity) then
      -- Name no activity => select by name
      Pers_Mng.Select_By_Name (Pers_Def.The_Persons, Criteria.Name,
                               First_Pers, Last_Pers);
    else
      -- Name and activity set => one person
      Pers_Mng.Search (Pers_Def.The_Persons, Criteria.Name, Criteria.Activity,
                       First_Pers);
      Last_Pers := First_Pers;
    end if;

    -- For each person, list the files
    Pers_Def.The_Persons.Move_At (First_Pers);
    for I in First_Pers .. Last_Pers loop
      -- Get person's pid
      Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
      -- Build ????????.<pid>
      File_Name := Mesu_Nam.Build_File_Name (Pid => Str_Mng.Pid_Str (Person.Pid));
      -- Add to file list
      Dir_Mng.List_Dir (The_Files, "", File_Name);

      -- Next person
      if I /= Last_Pers then
        Pers_Def.The_Persons.Move_To;
      end if;
    end loop;

    if The_Files.Is_Empty then
      -- No new file. Pos not affected and file list is empty.
      return;
    end if;


    -- Add files in line list
    The_Files.Rewind;
    loop
      The_Files.Read (File, Dir_Mng.File_List_Mng.Current);
      File_Name := File.Name.Image;
      Mesu_Nam.Split_File_Name (File_Name, Date_S, No_S, Pid_S);
      -- check date
     Ok := Date_Match (Date_S, Criteria.Date_Aft, Criteria.Date_Bef);

      if Ok then
        -- check pairs
        -- Get person & mesure to build afpx line rec
        Pers_Mng.Search (Pers_Def.The_Persons,
                         Pers_Def.Pid_Range'Value(Pid_S), First_Pers);
        Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
        Mesure := Mesu_Fil.Load (File_Name);
        Str_Mng.Format_Mesure_To_List (Person, Mesure, No_S, Line);

        if not Line_List.Is_Empty then
          Pos := Line_List.Get_Position;
          Ok := not File_Search (Line_List, Line, Line_List_Mng.Next, 1,
                                 Line_List_Mng.Absolute);
          Line_List.Move_At (Pos);
        end if;
      end if;

      -- Merge
      if Ok then
        Line_List.Insert (Line);
      end if;

      -- Next file
      exit when not The_Files.Check_Move;
      The_Files.Move_To;
    end loop;

    -- sort by name activity date
    File_Sort (Line_List);

    -- restore / set pos
    if Saved_Pos /= 0 then
      Line_List.Move_At (Saved_Pos);
    elsif not Line_List.Is_Empty then
      -- List was empty, move to first
      Line_List.Rewind;
    end if;

    -- Delete files list
    The_Files.Delete_List;
  end Add_Selection;

  -- Remove records from selection
  procedure Rem_Selection (Criteria : in Criteria_Rec) is
    Saved_Pos, Curr_Pos : Positive;
    Line   : Afpx.Line_Rec;
    Ok : Boolean;
    File_Name : Mesu_Nam.File_Name_Str;
    Date_S : Mesu_Nam.File_Date_Str;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;
    Pos_Pers : Positive;
    Person : Pers_Def.Person_Rec;
  begin
    -- Save current position
    if Line_List.Is_Empty then
      return;
    else
      Saved_Pos := Line_List.Get_Position;
    end if;
    -- Save list
    Save_List;

    -- for each in list
    Line_List.Rewind;
    loop
      -- Get line, file_name, split
      Line_List.Read (Line, Line_List_Mng.Current);
      Str_Mng.Format_List_To_Mesure (Line, File_Name);
      Mesu_Nam.Split_File_Name (File_Name, Date_S, No_S, Pid_S);

      Ok := True;
      if not Str_Mng.Is_Spaces (Criteria.Name) then
        -- Person name set : Get person and check names
        Pers_Mng.Search (Pers_Def.The_Persons, Pers_Def.Pid_Range'Value(Pid_S),
                         Pos_Pers);
        Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
        Ok := Person.Name = Criteria.Name;
        if Ok and then not Str_Mng.Is_Spaces (Criteria.Activity) then
          -- Activity set : check activity
          Ok := Person.Activity = Criteria.Activity;
        end if;
      end if;

      -- Check date
      if Ok then
        Ok := Date_Match (Date_S, Criteria.Date_Aft, Criteria.Date_Bef);
      end if;

      -- Delete line. Update saved pos if deleting initial current line
      if Ok then
        Curr_Pos := Line_List.Get_Position;
        if Curr_Pos /= Line_List.List_Length then
          Line_List.Delete;
          if Curr_Pos < Saved_Pos then
            Saved_Pos := Saved_Pos - 1;
          elsif Curr_Pos = Saved_Pos then
            Saved_Pos := Line_List.Get_Position;
          end if;
        else
          Line_List.Delete (Line_List_Mng.Prev);
          if Curr_Pos = Saved_Pos and then not Line_List.Is_Empty then
            Saved_Pos := Line_List.Get_Position;
          end if;
          exit;
        end if;
      else
        -- Next line except if list empty or end of list
        exit when Line_List.Is_Empty
        or else not Line_List.Check_Move;
        Line_List.Move_To;
      end if;

    end loop;

    -- Restore pos
    if not Line_List.Is_Empty then
      Line_List.Move_At (Saved_Pos);
    end if;

  end Rem_Selection;

  -- Add a record to selection
  procedure Add_Selection (Name : in Mesu_Nam.File_Name_Str) is
    Line   : Afpx.Line_Rec;
    Date_S : Mesu_Nam.File_Date_Str;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;
    Pos_Pers : Positive;
    Person : Pers_Def.Person_Rec;
    Mesure : Mesu_Def.Mesure_Rec;
    Dummy : Boolean;
    pragma Unreferenced (Dummy);
  begin

    Mesu_Nam.Split_File_Name (Name, Date_S, No_S, Pid_S);

    -- Get person and mesure
    Pers_Mng.Search (Pers_Def.The_Persons,
                     Pers_Def.Pid_Range'Value(Pid_S), Pos_Pers);
    Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
    Mesure := Mesu_Fil.Load (Name);

    -- Build line
    Str_Mng.Format_Mesure_To_List (Person, Mesure, No_S, Line);

    -- Insert
    Line_List.Insert (Line);

    -- Sort by name activity date
    File_Sort (Line_List);

    -- Current set to inserted
    Dummy := File_Search (Line_List, Line, Line_List_Mng.Next, 1,
                          Line_List_Mng.Absolute);
  end Add_Selection;

  -- Remove a record from selection
  procedure Rem_Selection (Name : in Mesu_Nam.File_Name_Str) is
    Saved_Pos, Curr_Pos : Positive;
    Line   : Afpx.Line_Rec;
    Date_S : Mesu_Nam.File_Date_Str;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;
    Pos_Pers : Positive;
    Person : Pers_Def.Person_Rec;
    Mesure : Mesu_Def.Mesure_Rec;
  begin
    -- Save current position
    if Line_List.Is_Empty then
      return;
    else
      Saved_Pos := Line_List.Get_Position;
    end if;

    -- Save list
    Save_List;

    -- Split file name
    Mesu_Nam.Split_File_Name (Name, Date_S, No_S, Pid_S);

    -- Get person and mesure
    Pers_Mng.Search (Pers_Def.The_Persons,
                     Pers_Def.Pid_Range'Value(Pid_S), Pos_Pers);
    Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
    Mesure := Mesu_Fil.Load (Name);

    -- Build line
    Str_Mng.Format_Mesure_To_List (Person, Mesure, No_S, Line);

    -- Search record
    if not File_Search (Line_List, Line, Line_List_Mng.Next, 1,
                        Line_List_Mng.Absolute) then
      Line_List.Move_At (Saved_Pos);
      return;
    end if;

    -- Delete line. Update saved pos if deleting initial current line
    Curr_Pos := Line_List.Get_Position;
    if Curr_Pos /= Line_List.List_Length then
      Line_List.Delete;
      if Curr_Pos < Saved_Pos then
        Saved_Pos := Saved_Pos - 1;
      elsif Curr_Pos = Saved_Pos then
        Saved_Pos := Line_List.Get_Position;
      end if;
    else
      Line_List.Delete (Line_List_Mng.Prev);
      if Curr_Pos = Saved_Pos and then not Line_List.Is_Empty then
        Saved_Pos := Line_List.Get_Position;
      end if;
    end if;
    if not Line_List.Is_Empty then
      Line_List.Move_At (Saved_Pos);
    end if;

  end Rem_Selection;

  -- Remove a record from selection
  procedure Rem_Selection (Line : in Afpx.Line_Rec) is
    Saved_Pos, Curr_Pos : Positive;
  begin
    -- Save current position
    if Line_List.Is_Empty then
      return;
    else
      Saved_Pos := Line_List.Get_Position;
    end if;

    -- Save list
    Save_List;

    -- Search record
    if not File_Search (Line_List, Line, Line_List_Mng.Next, 1,
                        Line_List_Mng.Absolute) then
      Line_List.Move_At (Saved_Pos);
      return;
    end if;

    -- Delete line. Update saved pos if deleting initial current line
    Curr_Pos := Line_List.Get_Position;
    if Curr_Pos /= Line_List.List_Length then
      Line_List.Delete;
      if Curr_Pos < Saved_Pos then
        Saved_Pos := Saved_Pos - 1;
      elsif Curr_Pos = Saved_Pos then
        Saved_Pos := Line_List.Get_Position;
      end if;
    else
      Line_List.Delete (Line_List_Mng.Prev);
      if Curr_Pos = Saved_Pos and then not Line_List.Is_Empty then
        Saved_Pos := Line_List.Get_Position;
      end if;
    end if;
    if not Line_List.Is_Empty then
      Line_List.Move_At (Saved_Pos);
    end if;

  end Rem_Selection;


  procedure Close is
  begin
    List_Io.Close (List_File);
  exception
    when others => null;
  end Close;

  -- Load the selection from file
  procedure Load is
    File_Name : Mesu_Nam.File_Name_Str;
    use List_Io;
  begin
    Line_List.Delete_List;
    -- Open file
    begin
      Open (List_File, In_File, List_File_Name);
    exception
      when Name_Error =>
        return;
    end;

    -- Read file
    while not End_Of_File (List_File) loop
      Read (List_File, File_Name);
      Add_Selection (File_Name);
    end loop;

    Close;

  end Load;


  -- Save the selection to file
  procedure Save is
    Saved_Pos  : Positive;
    Line   : Afpx.Line_Rec;
    File_Name : Mesu_Nam.File_Name_Str;
    use List_Io;
  begin
    -- Delete previous file
    begin
      Open (List_File, In_File, List_File_Name);
      Delete (List_File);
    exception
      when Name_Error => null;
    end;

    -- Create file
    Create (List_File, Out_File, List_File_Name);

    -- Save current position
    if Line_List.Is_Empty then
      Close;
      return;
    else
      Saved_Pos := Line_List.Get_Position;
    end if;

    Line_List.Rewind;
    -- Copy items
    loop
      Line_List.Read (Line, Line_List_Mng.Current);
      Str_Mng.Format_List_To_Mesure (Line, File_Name);
      Write (List_File, File_Name);

      exit when not Line_List.Check_Move;
      Line_List.Move_To;
    end loop;

    -- Restore pos, set it in saved_list
    Line_List.Move_At (Saved_Pos);

    Close;
  end Save;

  -- Undo (if possible) previous action on selection
  procedure Undo is
  begin
    Copy_List (From => Saved_List, To => Line_List);
  end Undo;

end Mesu_Sel;

