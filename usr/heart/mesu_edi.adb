with Con_Io, Afpx.Utils, Day_Mng, Normal, Normalization, Rounds;
with Pers_Def, Pers_Mng, Mesu_Def, Mesu_Fil, Mesu_Imp, Str_Mng, Afpx_Xref;
-- with Mesu_Nam;
-- Edition, Creation, deletion of mesure
package body Mesu_Edi is

  procedure Encode_Date (Date : in Mesu_Def.Date_Str) is
    Date_R : Str_Mng.Date_Str_Rec;
  begin
    Str_Mng.To_Rec (Date, Date_R);
    Afpx.Encode_Field (Afpx_Xref.Records.Day, (00, 00), Date_R.Day);
    Afpx.Encode_Field (Afpx_Xref.Records.Month, (00, 00), Date_R.Month);
    Afpx.Encode_Field (Afpx_Xref.Records.Year, (00, 00), Date_R.Year);
  end Encode_Date;

  -- Encode the Position or the Duration line (if Index=0)
  procedure Encode_Time (Index : in Natural;
                         Mesure : in Mesu_Def.Mesure_Rec) is
    Field : Afpx.Field_Range;
    Ind : Natural;
    Days : Natural;
    Seconds : Natural;
    Nb_Secs_Per_Day : constant := 60 * 60 * 24;
    H : Day_Mng.T_Hours;
    M : Day_Mng.T_Minutes;
    S : Day_Mng.T_Seconds;
    L : Day_Mng.T_Millisecs;
    use type Afpx.Absolute_Field_Range;
  begin
    if Index = 0 then
      -- Index=0 means total duration
      Field := Afpx_Xref.Records.Dur_Title;
      Ind := Mesure.Samples.Length;
    else
      Field := Afpx_Xref.Records.Pos_Title;
      Ind := Index;
    end if;
    -- Clear and done if less than 2 samples
    for Fld in Field .. Field + 7 loop
      Afpx.Set_Field_Activation (Fld, Mesure.Samples.Length >= 2);
    end loop;
    if Mesure.Samples.Length < 2 then
      return;
    end if;
    -- Compute duration in seconds, split and encode
    Seconds := Ind * Natural (Mesure.Sampling_Delta);
    Days := Seconds / Nb_Secs_Per_Day;
    Seconds := Seconds - Days * Nb_Secs_Per_Day;

    -- Put Days if not 0
    Afpx.Set_Field_Activation (Field + 1, Days /= 0);
    Afpx.Set_Field_Activation (Field + 2, Days /= 0);
    if Days /= 0 then
      Afpx.Encode_Field (Field + 1, (0, 0), Normal (Days, 3, Gap => ' ' ));
    end if;

    -- Put H M and S
    Day_Mng.Split (Duration (Seconds), H, M, S, L);
    Afpx.Encode_Field (Field + 3, (0, 0), Normal (H, 2, Gap => '0'));
    Afpx.Encode_Field (Field + 5, (0, 0), Normal (M, 2, Gap => '0'));
    Afpx.Encode_Field (Field + 7, (0, 0), Normal (S, 2, Gap => '0'));
  end Encode_Time;

  -- Encode the Duration line
  procedure Encode_Duration (Mesure : in Mesu_Def.Mesure_Rec) is
  begin
    Encode_Time (0, Mesure);
  end Encode_Duration;

  -- Encode the position line
  procedure Encode_Position (Mesure : in Mesu_Def.Mesure_Rec) is
    Index : Positive;
  begin
    if Afpx.Line_List.Is_Empty then
      -- Pos is not significant and must not be 0
      Index := 1;
    else
      Index := Positive (Afpx.Line_List.Get_Position);
    end if;
    Encode_Time (Index, Mesure);
  end Encode_Position;

  function Bpm_Line (Bpm : Pers_Def.Bpm_Range) return Afpx.Line_Rec is
    Line : Afpx.Line_Rec;
  begin
    Afpx.Encode_Line (Line, Str_Mng.To_Str (Bpm));
    return Line;
  end Bpm_Line;

  -- Encode the full screen
  procedure Encode (Person : in Pers_Def.Person_Rec;
                    Mesure : in Mesu_Def.Mesure_Rec) is
    use type Afpx.Absolute_Field_Range;
  begin
    -- Person name and activity
    Afpx.Encode_Field (Afpx_Xref.Records.Person, (00, 00), Person.Name);
    Afpx.Encode_Field (Afpx_Xref.Records.Activity, (00, 00), Person.Activity);

    -- Tz, Date, comment, sampling
    for I in Pers_Def.Person_Tz_Array'Range loop
      Afpx.Encode_Field (Afpx.Field_Range(I) + Afpx_Xref.Records.Tz1 - 1,
                         (00, 00),
                         Str_Mng.To_Str(Mesure.Tz(I)));
    end loop;

    -- Date, comment and sampling delta
    Encode_Date (Mesure.Date);
    Afpx.Encode_Field (Afpx_Xref.Records.Comment, (00, 00), Mesure.Comment);
    Afpx.Encode_Field (Afpx_Xref.Records.Sampling, (00, 00),
                       Str_Mng.To_Str (Mesure.Sampling_Delta));
    Encode_Duration (Mesure);
    Encode_Position (Mesure);
    -- Samples
    Afpx.Line_List.Delete_List;
    for I in 1 .. Mesure.Samples.Length loop
      Afpx.Line_List.Insert (Bpm_Line (Mesure.Samples.Element (I)));
    end loop;
    Afpx.Line_List.Rewind (Check_Empty => False);
  end Encode;

  procedure Protect (Field_No : in Afpx.Absolute_Field_Range) is
  begin
    Afpx.Set_Field_Colors (Field_No,
                           Foreground => Con_Io.Color_Of ("Black"),
                           Background => Con_Io.Color_Of ("Light_Grey"));
    Afpx.Set_Field_Protection (Field_No, True);
  end Protect;

  -- Import sample data from file
  procedure Import_Samples (Mesure   : in out Mesu_Def.Mesure_Rec;
                            Ok       : out Boolean;
                            Date_Set : out Boolean) is
  begin
    Ok := False;
    Mesu_Imp.Import (Mesure, Ok, Date_Set);
  end Import_Samples;


  -- Edit a mesure.
  -- If date or person changes, then the file name may be affected.
  -- New file may be created but previous file is not deleted
  -- If File_Name is empty as input, then it is a creation and file name
  --  is affected
  procedure Edit (File_Name : in out Mesu_Nam.File_Name_Str) is
    In_Create : Boolean;
    Person : Pers_Def.Person_Rec;
    Pos_Pers : Integer;
    Date_S : Mesu_Nam.File_Date_Str;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;
    Bpm_S  : Str_Mng.Bpm_Str;
    Bpm : Pers_Def.Bpm_Range;
    Mesure : Mesu_Def.Mesure_Rec;
    Date_Set : Boolean;
    Prev_Fld : Afpx.Field_Range;

    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result : Afpx.Result_Rec;

    Ok : Boolean;

    use type Pers_Def.Bpm_Range;

    -- Encode TZ afpx fields from Person
    procedure Encode_Tz is
      use type Afpx.Absolute_Field_Range;
    begin
      -- Encode Tz
      for I in Pers_Def.Person_Tz_Array'Range loop
        Afpx.Encode_Field (Afpx_Xref.Records.Tz1 + Afpx.Field_Range(I) - 1,
                          (00,00),
                           Str_Mng.To_Str(Person.Tz(I)));
      end loop;
    end Encode_Tz;

    -- Check a field
    procedure Check_Field (Current_Field : in out Afpx.Absolute_Field_Range;
                           For_Valid : in Boolean;
                           Move_Ok : in Boolean;
                           Ok : out Boolean) is
      Date_R : Str_Mng.Date_Str_Rec;
      Delta_Str : String (1 .. 3);
      Locok : Boolean;
      use type Afpx.Absolute_Field_Range;
    begin
      case Current_Field is

        when Afpx_Xref.Records.Person | Afpx_Xref.Records.Activity =>
          -- In name or activity
          -- Expand name & activity
          Person.Name     := Afpx.Decode_Field (Afpx_Xref.Records.Person, 00);
          Person.Activity := Afpx.Decode_Field (Afpx_Xref.Records.Activity, 00);
          Pers_Mng.Expand (Pers_Def.The_Persons,
                           Person.Name, Person.Activity, Pos_Pers);

          Afpx.Encode_Field (Afpx_Xref.Records.Person, (00, 00),
                             Person.Name);
          Afpx.Encode_Field (Afpx_Xref.Records.Activity, (00, 00),
                             Person.Activity);
          -- Set pos in case of end
          if Pos_Pers > 0 then
            -- Unique person and activity
            Pers_Def.The_Persons.Move_At (Pos_Pers);
            Pers_Def.The_Persons.Read (Person,
                                       Pers_Def.Person_List_Mng.Current);
            -- Copy Pid
            Mesure.Pid := Person.Pid;
            if not For_Valid then
              -- Go to date
              if Move_Ok then
                -- Person and activity is validated
                -- Encode Tz from person
                Encode_Tz;
                -- Encode Sample delta from person
                Afpx.Encode_Field (Afpx_Xref.Records.Sampling, (00, 00),
                                   Str_Mng.To_Str (Person.Sampling_Delta));
                  Current_Field := Afpx_Xref.Records.Day;
               end if;
            else
              -- Next check : Tz
              if Move_Ok then
                Current_Field := Afpx_Xref.Records.Tz1;
               end if;
            end if;
            Locok := True;
          elsif Pos_Pers = 0 then
            -- Person OK
            if Move_Ok then
              Current_Field := Afpx_Xref.Records.Activity;
            end if;
            Locok := False;
          else
            -- Person KO
            if Move_Ok then
              Current_Field := Afpx_Xref.Records.Person;
            end if;
            Locok := False;
          end if;

        when Afpx_Xref.Records.Tz1 .. Afpx_Xref.Records.Tz6 =>
          -- In Tz check it
          Bpm_S := Afpx.Decode_Field (Current_Field, 00);
          Str_Mng.Parse(Bpm_S);
          Locok := not Str_Mng.Has_Holes(Bpm_S);
          if Locok then
            begin
              Mesure.Tz(Integer(Current_Field - Afpx_Xref.Records.Tz1 + 1))
                        := Str_Mng.To_Bpm(Bpm_S);
            exception
              when others =>
                Locok := False;
            end;
          end if;
          if Locok and then Move_Ok then
            if Current_Field /= Afpx_Xref.Records.Tz6 then
              Current_Field := Current_Field + 1;
            else
              Current_Field := Afpx_Xref.Records.Day;
            end if;
          end if;

        when Afpx_Xref.Records.Day
           | Afpx_Xref.Records.Month
           | Afpx_Xref.Records.Year =>
          -- In date
          -- Check date : no space
          Current_Field := Afpx_Xref.Records.Day;
          Date_R.Day   := Afpx.Decode_Field (Afpx_Xref.Records.Day, 00);
          Locok := not Str_Mng.Has_Spaces(Date_R.Day);
          if Locok then
            if Move_Ok then
              Current_Field := Afpx_Xref.Records.Month;
            end if;
            Date_R.Month := Afpx.Decode_Field (Afpx_Xref.Records.Month, 00);
            Locok := not Str_Mng.Has_Spaces(Date_R.Month);
          end if;
          if Locok then
            if Move_Ok then
              Current_Field := Afpx_Xref.Records.Year;
            end if;
            Date_R.Year  := Afpx.Decode_Field (Afpx_Xref.Records.Year, 00);
            Locok := not Str_Mng.Has_Spaces(Date_R.Year);
          end if;

          -- Check date : valid
          if Locok then
            if Move_Ok then
              Current_Field := Afpx_Xref.Records.Day;
            end if;
            Str_Mng.Check_Date (Date_R, True, Mesure.Date, Locok);
          end if;
          if Locok then
            if Move_Ok then
              Current_Field := Afpx_Xref.Records.Comment;
            end if;
          end if;

        when Afpx_Xref.Records.Comment =>
          -- In comment, replace non ASCII chars by '#'
          declare
            Unicodes : constant Afpx.Unicode_Sequence
                     := Afpx.Decode_Field (Afpx_Xref.Records.Comment, 00);
          begin
            Mesure.Comment := (others => ' ');
            for I in Unicodes'Range loop
              if Unicodes(I) <= 127 then
                Mesure.Comment(I) := Character'Val (Unicodes(I));
              else
                Mesure.Comment(I) := '#';
              end if;
            end loop;
          end;
          if Move_Ok then
            Current_Field := Afpx_Xref.Records.Sampling;
          end if;

        when Afpx_Xref.Records.Sampling =>
          -- In sampling delta
          Delta_Str := Afpx.Decode_Field(Afpx_Xref.Records.Sampling, 00);
          Str_Mng.Parse (Delta_Str);
          -- No hole no space
          Locok := not Str_Mng.Has_Holes (Delta_Str);
          Locok := Locok and then not Str_Mng.Is_Spaces (Delta_Str);
          if Locok then
            begin
              Mesure.Sampling_Delta := Str_Mng.To_Sampling (Delta_Str);
            exception
              when others =>
                Locok := False;
            end;
          end if;
          if Locok then
            Encode_Duration (Mesure);
            Encode_Position (Mesure);
            if Move_Ok then
              Current_Field := Afpx_Xref.Records.Person;
            end if;
          end if;

        when others =>
          null;

      end case;

      Get_Handle.Cursor_Col := 0;
      Get_Handle.Insert := False;
      Ok := Locok;
    end Check_Field;

    -- Cursor in Get fied
    function Cursor_Set_Col_Cb (
        Cursor_Field : Afpx.Field_Range;
        Dummy_New_Field : Boolean;
        Pointer_Col : Con_Io.Col_Range;
        Dummy_Offset : Con_Io.Col_Range;
        Enter_Field_Cause : Afpx.Enter_Field_Cause_List;
        Dummy_Str : Afpx.Unicode_Sequence) return Con_Io.Col_Range is
      use type Afpx.Absolute_Field_Range;
    begin
      -- Reset is available only when in TZs
      Afpx.Set_Field_Activation (Afpx_Xref.Records.Reset,
                   Cursor_Field >= Afpx_Xref.Records.Tz1
          and then Cursor_Field <= Afpx_Xref.Records.Tz6);
      -- Check previous field but no move if OK
      Check_Field (Prev_Fld, False, False, Ok);
      Prev_Fld := Cursor_Field;
      return Afpx.Default_Cursor_Col (Cursor_Field,
                                      Pointer_Col,
                                      Enter_Field_Cause);
    end Cursor_Set_Col_Cb;

    -- Normalize Afpx list index
    function Normal is new Normalization.Normal_Mod
      (Afpx.Line_List_Mng.Ll_Natural);
    -- Update the list status
    procedure List_Change (Dummy_Action : in Afpx.List_Change_List;
                           Status : in Afpx.List_Status_Rec) is
      -- Left and Right selection in list
      Left  : constant Afpx.Line_List_Mng.Ll_Natural
            := Status.Ids_Selected (Afpx.List_Left);
      Empty : constant Boolean := Mesure.Samples.Is_Null;
      Percent : Afpx.Percent_Range;
      Row : Con_Io.Row_Range;
    begin
      -- Protect Suppr and Clear_All buttons if empty list
      Afpx.Set_Field_Activation (Afpx_Xref.Records.Suppr, not Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Records.Clear_All, not Empty);
      -- Put percent value and "scroll bar"
      Percent := Afpx.Get_List_Percent;
      Afpx.Clear_Field (Afpx_Xref.Records.Scroll);
      if Percent /= 0 then
        Afpx.Encode_Field (Afpx_Xref.Records.Percent, (0, 0),
                           Normal (Percent, 3, True));
        -- 0 <-> 1% and Height-1 <-> 100%
        -- (Percent-1)/99 = Row/(Height-1)
        Row := Con_Io.Row_Range(
          Rounds.Roundiv ((Afpx.Get_Field_Height (Afpx_Xref.Records.Scroll) - 1)
                          * (Percent - 1), 99));
        Afpx.Encode_Field (Afpx_Xref.Records.Scroll,
                          (Row => Row, Col => 0),
                          "-");
      else
        Afpx.Encode_Field (Afpx_Xref.Records.Percent, (0, 0), "-");
      end if;
      -- Put Ids selected
      Afpx.Encode_Field (Afpx_Xref.Records.Sel, (0, 0), Normal (
          Left, Afpx.Get_Field_Width (Afpx_Xref.Records.Sel), False));
      Encode_Position (Mesure);
    end List_Change;

    -- Move according to click row in scroll field
    procedure Move_At_Scroll (Row : in Con_Io.Row_Range) is
      Percent : Afpx.Percent_Range;
      Position : Afpx.Line_List_Mng.Ll_Natural;
      Saved_Position : Afpx.Utils.Backup_Context;
      use type Afpx.Line_List_Mng.Ll_Natural;
    begin
      if Afpx.Line_List.Is_Empty then
        return;
      end if;
      -- 0 <-> 1% and Height-1 <-> 100%
      -- (Percent-1)/99 = Row/(Height-1)
      Percent :=
          Rounds.Roundiv (Row * 99,
                          Afpx.Get_Field_Height (Afpx_Xref.Records.Scroll) - 1)
          + 1;
      Position := Afpx.Get_List_Index (Percent);
      if Position = 0 then
        return;
      end if;
      -- Center to selected index but keep unchanged selected line
      Saved_Position.Backup;
      Afpx.Line_List.Move_At (Position);
      Afpx.Update_List (Afpx.Top_Selected);
      Saved_Position.Restore (Force_Position => True);
    end Move_At_Scroll;

    -- Init screen
    procedure Init (Reset : in Boolean) is
    begin
      -- Use descriptor
      Afpx.Use_Descriptor (Afpx_Xref.Records.Dscr_Num);
      In_Create := Str_Mng.Is_Spaces (File_Name);
      -- Title and cursor field
      if In_Create then
        Afpx.Encode_Field (Afpx_Xref.Records.Title, (00,00), "Creation");
        -- Person name
        Get_Handle.Cursor_Field := Afpx_Xref.Records.Person;
      else
        Afpx.Encode_Field (Afpx_Xref.Records.Title, (00,00), " Edition");
        -- Date
        Get_Handle.Cursor_Field := Afpx_Xref.Records.Day;
      end if;
      Afpx.Set_Field_Activation (Afpx_Xref.Records.Reset, False);
      Get_Handle.Cursor_Col := 0;
      Get_Handle.Insert := False;
      Prev_Fld := Afpx.Next_Cursor_Field (0);

      if Reset then
        -- Reset / reload data
        if In_Create then
          -- Set person and mesure
          Person.Name := (others => ' ');
          Person.Activity := (others => ' ');
          Mesure := (others => <>);
          Mesure.Date := Str_Mng.Current_Date;
        else
          -- Load person
          Mesu_Nam.Split_File_Name (File_Name, Date_S, No_S, Pid_S);
          Person.Pid := Pers_Def.Pid_Range'Value(Pid_S);
          Pers_Mng.Search (Pers_Def.The_Persons, Person.Pid, Pos_Pers);
          Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
          -- Load mesure
          Mesure := Mesu_Fil.Load (File_Name);
        end if;
      end if;

      Encode (Person, Mesure);
    end Init;

    use type Afpx.Absolute_Field_Range;
  begin
    Init (True);

    -- Loop of Ptgs
    loop
      Afpx.Encode_Field (Afpx_Xref.Records.Date, (00, 00),
                         Str_Mng.Current_Date_Printed);
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
          Cursor_Col_Cb => Cursor_Set_Col_Cb'Access,
          List_Change_Cb => List_Change'Access);

      case Ptg_Result.Event is
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
        when Afpx.Keyboard =>

          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Check field and go to next if Ok
              Check_Field (Get_Handle.Cursor_Field, False, True, Ok);

            when Afpx.Escape_Key =>
              -- Clear current field
              if Get_Handle.Cursor_Field = Afpx_Xref.Records.Person then
                Afpx.Clear_Field (Afpx_Xref.Records.Person);
                Afpx.Clear_Field (Afpx_Xref.Records.Activity);
                Get_Handle.Cursor_Field := Afpx_Xref.Records.Person;
              elsif Get_Handle.Cursor_Field = Afpx_Xref.Records.Activity then
                Afpx.Clear_Field (Afpx_Xref.Records.Activity);
              elsif Get_Handle.Cursor_Field = Afpx_Xref.Records.Day
              or else Get_Handle.Cursor_Field = Afpx_Xref.Records.Month
              or else Get_Handle.Cursor_Field = Afpx_Xref.Records.Year then
                Afpx.Clear_Field (Afpx_Xref.Records.Day);
                Afpx.Clear_Field (Afpx_Xref.Records.Month);
                Afpx.Clear_Field (Afpx_Xref.Records.Year);
                Get_Handle.Cursor_Field := Afpx_Xref.Records.Day;
              else
                Afpx.Clear_Field (Get_Handle.Cursor_Field);
              end if;
              Get_Handle.Cursor_Col := 0;
              Get_Handle.Insert := False;
            when Afpx.Break_Key =>
              raise Pers_Def.Exit_Requested;
          end case;

        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx_Xref.Records.Top .. Afpx_Xref.Records.Bottom =>
              Afpx.Utils.Scroll (Ptg_Result.Field_No
                                 - Afpx_Xref.Records.Top + 1);

            when Afpx_Xref.Records.Import =>
              -- Import samples
              -- Valid check all fields one by one from person to 1st sample
              Get_Handle.Cursor_Field := Afpx_Xref.Records.Person;
              loop
                Check_Field (Get_Handle.Cursor_Field, True, True, Ok);
                exit when not Ok
                or else Get_Handle.Cursor_Field = Afpx_Xref.Records.Sampling;
              end loop;
              if Ok then
                Import_Samples (Mesure, Ok, Date_Set);
                -- Restore screen
                Init (False);
                Encode (Person, Mesure);
                Get_Handle.Cursor_Field :=
                    (if Date_Set then Afpx_Xref.Records.Day
                     else Afpx_Xref.Records.Sampling);
              end if;

            when Afpx_Xref.Records.Cancel =>
              -- Cancel
              File_Name := Mesu_Nam.File_Name_Str'((others => ' '));
              exit;

            when Afpx_Xref.Records.Valid =>
              -- Valid check all fields one by one
              Get_Handle.Cursor_Field := Afpx_Xref.Records.Person;
              loop
                Check_Field (Get_Handle.Cursor_Field, True, True, Ok);
                exit when not Ok
                or else Get_Handle.Cursor_Field = Afpx_Xref.Records.Person;
              end loop;

              -- Check no hole in Tz, crescent
              if Ok then
                for I in Pers_Def.Person_Tz_Array'Range loop
                  Get_Handle.Cursor_Field := Afpx_Xref.Records.Tz1
                                           + Afpx.Field_Range(I) - 1;
                  if Pers_Def."=" (Mesure.Tz(I), Pers_Def.Bpm_Range'First) then
                    Ok := False;
                    exit;
                  end if;
                  if I /= Pers_Def.Person_Tz_Array'First
                  and then Mesure.Tz(I - 1) >= Mesure.Tz(I) then
                    Ok := False;
                    exit;
                  end if;
                end loop;
              end if;

              -- Delete if needed, then find slot if needed
              if Ok then
                -- Check if file to be deleted
                if      In_Create
                or else Mesure.Date /= Date_S
                or else Pid_S /= Str_Mng.Pid_Str(Mesure.Pid) then
                  -- Necessity to create a new file_name.
                  -- Build new file name (find_slot) -> set No_S
                  Pid_S := Str_Mng.Pid_Str(Mesure.Pid);
                  No_S := Mesu_Nam.Find_Slot (Date => Mesure.Date,
                                              Pid  => Pid_S);
                  -- Ok if an empty slot is found. Build file name
                  Ok := No_S /= Mesu_Nam.Wild_No_Str;
                  if Ok then
                    File_Name :=
                      Mesu_Nam.Build_File_Name (Mesure.Date, No_S, Pid_S);
                  end if;
                end if;
              end if;

              if Ok then
                -- Save
                Mesu_Fil.Save (No_S, Mesure);
                exit;
              end if;

            when Afpx_Xref.Records.Scroll =>
              Move_At_Scroll (Ptg_Result.Release_Pos.Row);

            when Afpx_Xref.Records.Ins_Bef =>
              -- Insert a sample
              Bpm := Str_Mng.To_Bpm (Afpx.Decode_Field (
                  Afpx_Xref.Records.Ins, 0));
              if Bpm /= Pers_Def.No_Bpm then
                if Mesure.Samples.Is_Null then
                  Mesure.Samples.Append (Bpm);
                else
                  Mesure.Samples.Insert (Positive (Afpx.Line_List.Get_Position),
                                         Bpm);
                end if;
                Afpx.Line_List.Insert (Bpm_Line (Bpm), Afpx.Line_List_Mng.Prev);
                Encode_Duration (Mesure);
              end if;

            when Afpx_Xref.Records.Ins_Aft =>
              -- Insert a sample
              Bpm := Str_Mng.To_Bpm (Afpx.Decode_Field (
                  Afpx_Xref.Records.Ins, 0));
              if Bpm /= Pers_Def.No_Bpm then
                if Mesure.Samples.Is_Null then
                  Mesure.Samples.Append (Bpm);
                else
                  Mesure.Samples.Insert (
                      Positive (Afpx.Line_List.Get_Position) + 1,
                      Bpm);
                end if;
                Afpx.Line_List.Insert (Bpm_Line (Bpm), Afpx.Line_List_Mng.Next);
                Encode_Duration (Mesure);
              end if;

            when Afpx_Xref.Records.Suppr =>
              -- Suppress a sample
              Mesure.Samples.Delete_Nb (Positive (Afpx.Line_List.Get_Position),
                                        1);
              Afpx.Line_List.Delete;
              Encode_Duration (Mesure);

            when Afpx_Xref.Records.Clear_All =>
              -- Clear all the samples
              Mesure.Samples.Set_Null;
              Afpx.Line_List.Delete_List;
              Encode_Duration (Mesure);

            when Afpx_Xref.Records.Reset =>
              if Get_Handle.Cursor_Field >=  Afpx_Xref.Records.Tz1
              and then Get_Handle.Cursor_Field <= Afpx_Xref.Records.Tz6 then
                -- Reset of Tz from Person (if any, if not => clear)
                Encode_Tz;
              end if;

            when others =>
              null;
            end case; -- Ptg_Result.Field_No

      end case; -- Result.Event

    end loop;

  end Edit;

  -- Clone a mesure: create a new file name an edit it
  procedure Clone (File_Name : in out Mesu_Nam.File_Name_Str) is
    Mesure : Mesu_Def.Mesure_Rec;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;

  begin
    -- Fill data from origin
    Mesure := Mesu_Fil.Load (File_Name);
    -- Set date and reset samples
    Mesure.Date := Str_Mng.Current_Date;
    Mesure.Samples.Set_Null;
    -- Create new file
    Pid_S := Str_Mng.Pid_Str(Mesure.Pid);
    No_S := Mesu_Nam.Find_Slot (Mesure.Date, Pid_S);
    -- Ok if an empty slot is found. Build file name
    if No_S = Mesu_Nam.Wild_No_Str then
      File_Name := (others => ' ');
      return;
    end if;
    File_Name := Mesu_Nam.Build_File_Name (Mesure.Date, No_S, Pid_S);
    -- Save and edit
    Mesu_Fil.Save (No_S, Mesure);
    Edit (File_Name);
  end Clone;

  -- Delete a mesure
  -- File is not deleted
  procedure Delete (File_Name : in out Mesu_Nam.File_Name_Str) is
    Person : Pers_Def.Person_Rec;
    Pos_Pers : Integer;
    Date_S : Mesu_Nam.File_Date_Str;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;
    Mesure : Mesu_Def.Mesure_Rec;

    Get_Handle : Afpx.Get_Handle_Rec;
    Ptg_Result : Afpx.Result_Rec;

    use type Afpx.Absolute_Field_Range;

    -- Normalize Afpx list index
    function Normal is new Normalization.Normal_Mod
      (Afpx.Line_List_Mng.Ll_Natural);
    -- Update the list status
    procedure List_Change (Dummy_Action : in Afpx.List_Change_List;
                           Status : in Afpx.List_Status_Rec) is
      -- Left and Right selection in list
      Left  : constant Afpx.Line_List_Mng.Ll_Natural
            := Status.Ids_Selected (Afpx.List_Left);
      Percent : Afpx.Percent_Range;
      Row : Con_Io.Row_Range;
    begin
      -- Put percent value and "scroll bar"
      Percent := Afpx.Get_List_Percent;
      Afpx.Clear_Field (Afpx_Xref.Records.Scroll);
      if Percent /= 0 then
        Afpx.Encode_Field (Afpx_Xref.Records.Percent, (0, 0),
                           Normal (Percent, 3, True));
        -- 0 <-> 1% and Height-1 <-> 100%
        -- (Percent-1)/99 = Row/(Height-1)
        Row := Con_Io.Row_Range(
          Rounds.Roundiv ((Afpx.Get_Field_Height (Afpx_Xref.Records.Scroll) - 1)
                          * (Percent - 1), 99));
        Afpx.Encode_Field (Afpx_Xref.Records.Scroll,
                          (Row => Row, Col => 0),
                          "-");
      else
        Afpx.Encode_Field (Afpx_Xref.Records.Percent, (0, 0), "-");
      end if;
      -- Put Id selected
      Afpx.Encode_Field (Afpx_Xref.Records.Sel, (0, 0), Normal (
          Left, Afpx.Get_Field_Width (Afpx_Xref.Records.Sel), False));
    end List_Change;

    -- Move according to click row in scroll field
    procedure Move_At_Scroll (Row : in Con_Io.Row_Range) is
      Percent : Afpx.Percent_Range;
      Position : Afpx.Line_List_Mng.Ll_Natural;
      Saved_Position : Afpx.Utils.Backup_Context;
      use type Afpx.Line_List_Mng.Ll_Natural;
    begin
      if Afpx.Line_List.Is_Empty then
        return;
      end if;
      -- 0 <-> 1% and Height-1 <-> 100%
      -- (Percent-1)/99 = Row/(Height-1)
      Percent :=
          Rounds.Roundiv (Row * 99,
                          Afpx.Get_Field_Height (Afpx_Xref.Records.Scroll) - 1)
          + 1;
      Position := Afpx.Get_List_Index (Percent);
      if Position = 0 then
        return;
      end if;
      -- Center to selected index but keep unchanged selected line
      Saved_Position.Backup;
      Afpx.Line_List.Move_At (Position);
      Afpx.Update_List (Afpx.Top_Selected);
      Saved_Position.Restore (Force_Position => True);
    end Move_At_Scroll;

  begin

    -- Load person
    Mesu_Nam.Split_File_Name (File_Name, Date_S, No_S, Pid_S);
    Person.Pid := Pers_Def.Pid_Range'Value(Pid_S);
    Pers_Mng.Search (Pers_Def.The_Persons, Person.Pid, Pos_Pers);
    Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
    -- Load mesure
    Mesure := Mesu_Fil.Load (File_Name);

    -- Use descriptor and encode
    Afpx.Use_Descriptor (Afpx_Xref.Records.Dscr_Num);
    Afpx.Encode_Field (Afpx_Xref.Records.Title, (00,00), "Deletion");
    Encode (Person, Mesure);
    Encode_Duration (Mesure);

    -- Protect fields
    -- Person name, activity, Tz
    Protect (Afpx_Xref.Records.Person);
    Protect (Afpx_Xref.Records.Activity);
    for I in Pers_Def.Person_Tz_Array'Range loop
      Protect (Afpx_Xref.Records.Tz1 + Afpx.Field_Range(I) - 1);
    end loop;
    -- Date, comment, samples
    Protect (Afpx_Xref.Records.Day);
    Protect (Afpx_Xref.Records.Month);
    Protect (Afpx_Xref.Records.Year);
    Protect (Afpx_Xref.Records.Comment);
    Protect (Afpx_Xref.Records.Sampling);
    Protect (Afpx.List_Field_No);

    -- Disable Import
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Import, False);

    -- Disable position and selection
    for Fld in Afpx_Xref.Records.Pos_Title .. Afpx_Xref.Records.Pos_Title + 7
    loop
      Afpx.Set_Field_Activation (Fld, False);
    end loop;
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Tsel, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Sel, False);

    -- Disable Ins, Suppr, ClearAll, TZReset abd Ins
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Ins_Bef, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Ins_Aft, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Suppr, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Clear_All, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Reset, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Ins, False);

    -- Loop of ptgs
    loop
      Afpx.Encode_Field (Afpx_Xref.Records.Date, (00, 00),
                         Str_Mng.Current_Date_Printed);
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result,
           List_Change_Cb => List_Change'Access);

      case Ptg_Result.Event is
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
        when Afpx.Keyboard =>
          case Ptg_Result.Keyboard_Key is
            when Afpx.Break_Key =>
              raise Pers_Def.Exit_Requested;
            when others =>
              null;
          end case;
        when Afpx.Mouse_Button =>
          case Ptg_Result.Field_No is
            when Afpx_Xref.Records.Top .. Afpx_Xref.Records.Bottom =>
              Afpx.Utils.Scroll (Ptg_Result.Field_No
                                 - Afpx_Xref.Records.Top + 1);
            when Afpx_Xref.Records.Scroll =>
              Move_At_Scroll (Ptg_Result.Release_Pos.Row);
            when Afpx_Xref.Records.Cancel =>
              -- Cancel
              File_Name := Mesu_Nam.File_Name_Str'((others => ' '));
              exit;
            when Afpx_Xref.Records.Valid =>
              -- OK. The caller will have to delete the file
              --  (after unselecting it)
              exit;
            when others =>
              null;
          end case; -- Ptg_Result.Field_No = valid or cancel
      end case; -- Result.Event

    end loop;

  end Delete;

end Mesu_Edi;

