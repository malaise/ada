with Con_Io, Afpx, Day_Mng, Normal;
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

  -- Encode the Duration line
  procedure Encode_Duration (Sampling_Delta: in Pers_Def.Sampling_Delta_Range)
  is
    Nb_Samples : Natural := 0;
    Seconds : Natural;
    H : Day_Mng.T_Hours;
    M : Day_Mng.T_Minutes;
    S : Day_Mng.T_Seconds;
    L : Day_Mng.T_Millisecs;
    use type Afpx.Absolute_Field_Range, Pers_Def.Bpm_Range;
  begin
    -- Count empty slots from last
    for Sample in reverse
             Afpx_Xref.Records.Rate_001
          .. Afpx_Xref.Records.Rate_001
           + Afpx.Field_Range (Mesu_Def.Sample_Nb_Range'Last) - 1
    loop
      exit when Str_Mng.To_Bpm (Afpx.Decode_Field (Sample, 0))
             /= Pers_Def.Bpm_Range'First;
      Nb_Samples := Nb_Samples + 1;
    end loop;
    -- Nb samples
    Nb_Samples := Mesu_Def.Sample_Nb_Range'Last - Nb_Samples;
    -- Clear if less than 2 samples
    for Fld in Afpx_Xref.Records.Dur_Title
            .. Afpx_Xref.Records.Dur_Sec loop
      Afpx.Set_Field_Activation (Fld, Nb_Samples >= 2);
    end loop;
    if Nb_Samples < 2 then
      return;
    end if;
    -- Compute duration in seconds, split and encode
    Seconds := Nb_Samples * Natural (Sampling_Delta);
    Day_Mng.Split (Duration (Seconds), H, M, S, L);
    Afpx.Encode_Field (Afpx_Xref.Records.Dur_Hour, (0, 0),
        Normal (H, 2, Gap => '0'));
    Afpx.Encode_Field (Afpx_Xref.Records.Dur_Min, (0, 0),
        Normal (M, 2, Gap => '0'));
    Afpx.Encode_Field (Afpx_Xref.Records.Dur_Sec, (0, 0),
        Normal (S, 2, Gap => '0'));

  end Encode_Duration;

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

    Encode_Date (Mesure.Date);
    Afpx.Encode_Field (Afpx_Xref.Records.Comment, (00, 00), Mesure.Comment);
    Afpx.Encode_Field (Afpx_Xref.Records.Sampling, (00, 00),
                       Str_Mng.To_Str (Mesure.Sampling_Delta));
    -- Samples
    for I in Mesu_Def.Sample_Nb_Range loop
      Afpx.Encode_Field (Afpx_Xref.Records.Rate_001 + Afpx.Field_Range(I) - 1,
                         (00, 00),
                         Str_Mng.To_Str(Mesure.Samples(I)) );
    end loop;
    Encode_Duration (Person.Sampling_Delta);
  end Encode;

  procedure Protect (Field_No : in Afpx.Field_Range) is
  begin
    Afpx.Set_Field_Colors (Field_No,
                           Foreground => Con_Io.Color_Of ("Black"),
                           Background => Con_Io.Color_Of ("Light_Grey"));
    Afpx.Set_Field_Protection (Field_No, True);
  end Protect;

  -- Import sample data from ascii file
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
    Mesure : Mesu_Def.Mesure_Rec;
    Date_Set : Boolean;
    Space_Found : Boolean;

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
            -- Uniq
            Pers_Def.The_Persons.Move_At (Pos_Pers);
            Pers_Def.The_Persons.Read (Person,
                                       Pers_Def.Person_List_Mng.Current);
            -- Copy Pid
            Mesure.Pid := Person.Pid;
            if not For_Valid then
              -- Encode Tz from person
              Encode_Tz;
              -- Encode Sample delta from person
              Afpx.Encode_Field (Afpx_Xref.Records.Sampling, (00, 00),
                                 Str_Mng.To_Str (Person.Sampling_Delta));
              -- Go to date
              Current_Field := Afpx_Xref.Records.Day;
            else
              -- Next check : Tz
              Current_Field := Afpx_Xref.Records.Tz1;
            end if;
            Locok := True;
          elsif Pos_Pers = 0 then
            Current_Field := Afpx_Xref.Records.Activity;
            Locok := False;
          else
            Current_Field := Afpx_Xref.Records.Person;
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
          if Locok then
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
            Current_Field := Afpx_Xref.Records.Month;
            Date_R.Month := Afpx.Decode_Field (Afpx_Xref.Records.Month, 00);
            Locok := not Str_Mng.Has_Spaces(Date_R.Month);
          end if;
          if Locok then
            Current_Field := Afpx_Xref.Records.Year;
            Date_R.Year  := Afpx.Decode_Field (Afpx_Xref.Records.Year, 00);
            Locok := not Str_Mng.Has_Spaces(Date_R.Year);
          end if;

          -- Check date : valid
          if Locok then
            Current_Field := Afpx_Xref.Records.Day;
            Str_Mng.Check_Date (Date_R, True, Mesure.Date, Locok);
          end if;
          if Locok then
            Current_Field := Afpx_Xref.Records.Comment;
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
          Current_Field := Afpx_Xref.Records.Sampling;

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
            Current_Field := Afpx_Xref.Records.Rate_001;
          end if;

        when Afpx_Xref.Records.Rate_001 .. Afpx_Xref.Records.Rate_120 =>
          -- In a sample
          Bpm_S := Afpx.Decode_Field (Current_Field, 0);
          Str_Mng.Parse (Bpm_S);
          -- No holes
          Locok := not Str_Mng.Has_Holes(Bpm_S);
          if Locok then
            begin
              Mesure.Samples(Mesu_Def.Sample_Nb_Range(
                              Current_Field - Afpx_Xref.Records.Rate_001 + 1))
                             := Str_Mng.To_Bpm(Bpm_S);
            exception
              when others =>
                Locok := False;
            end;
          end if;
          -- No empty
          if Locok then
            -- "Next" field
            if Current_Field /= Afpx_Xref.Records.Rate_120 then
              Current_Field := Current_Field + 1;
            else
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
      Encode_Duration (Person.Sampling_Delta);
      return Afpx.Default_Cursor_Col (Cursor_Field,
                                      Pointer_Col,
                                      Enter_Field_Cause);
    end Cursor_Set_Col_Cb;

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

      if Reset then
        -- Reset / reload data
        if In_Create then
          -- Set person and mesure
          Person.Name := (others => ' ');
          Person.Activity := (others => ' ');
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
          Cursor_Col_Cb => Cursor_Set_Col_Cb'Access);

      case Ptg_Result.Event is
        when Afpx.Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event
           | Afpx.Refresh =>
          null;
        when Afpx.Keyboard =>

          case Ptg_Result.Keyboard_Key is
            when Afpx.Return_Key =>
              -- Check field and go to next if Ok
              Check_Field (Get_Handle.Cursor_Field, False, Ok);
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
          if Ptg_Result.Field_No = Afpx_Xref.Records.Import then
            -- Import samples
            -- Valid check all fields one by one from person to 1st sample
            Get_Handle.Cursor_Field := Afpx_Xref.Records.Person;
            loop
              Check_Field (Get_Handle.Cursor_Field, True, Ok);
              exit when not Ok
              or else Get_Handle.Cursor_Field = Afpx_Xref.Records.Rate_001;
            end loop;
            if Ok then
              Import_Samples (Mesure, Ok, Date_Set);
              -- Restore screen
              Init (False);
              Encode (Person, Mesure);
              Get_Handle.Cursor_Field :=
                  (if Date_Set then Afpx_Xref.Records.Day
                   else Afpx_Xref.Records.Rate_001);
            end if;

          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Cancel then
            -- Cancel
            File_Name := Mesu_Nam.File_Name_Str'((others => ' '));
            exit;
          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Valid then
            -- Valid check all fields one by one
            Get_Handle.Cursor_Field := Afpx_Xref.Records.Person;
            loop
              Check_Field (Get_Handle.Cursor_Field, True, Ok);
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

            -- Check no hole in samples
            if Ok then
              Space_Found := False;
              for I in Mesu_Def.Sample_Nb_Range loop
                Get_Handle.Cursor_Field := Afpx_Xref.Records.Rate_001
                                         + Afpx.Field_Range(I) - 1;
                if Pers_Def."=" (Mesure.Samples(I), Pers_Def.Bpm_Range'First)
                then
                  Space_Found := True;
                elsif Space_Found then
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

          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Ins
                and then Get_Handle.Cursor_Field >= Afpx_Xref.Records.Rate_001
                and then Get_Handle.Cursor_Field <= Afpx_Xref.Records.Rate_120
          then
            -- Insert a sample
            for I in reverse Get_Handle.Cursor_Field + 1
                          .. Afpx_Xref.Records.Rate_120 loop
              Afpx.Encode_Field (I, (0, 0),
                 Afpx.Unicode_Sequence'(Afpx.Decode_Field(I - 1, 0)));
            end loop;
            Afpx.Clear_Field(Get_Handle.Cursor_Field);
            Encode_Duration (Person.Sampling_Delta);

          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Del
                and then Get_Handle.Cursor_Field >= Afpx_Xref.Records.Rate_001
                and then Get_Handle.Cursor_Field <= Afpx_Xref.Records.Rate_120
          then
            -- Suppress a sample
            for I in Get_Handle.Cursor_Field
                  .. Afpx_Xref.Records.Rate_120 - 1 loop
              Afpx.Encode_Field (I, (0, 0),
                 Afpx.Unicode_Sequence'(Afpx.Decode_Field(I + 1, 0)));
            end loop;
            Afpx.Clear_Field(Afpx_Xref.Records.Rate_120);
            Encode_Duration (Person.Sampling_Delta);

          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Clear then
            -- Clear all the samples
            for I in Afpx_Xref.Records.Rate_001
                  .. Afpx_Xref.Records.Rate_120 loop
              Afpx.Clear_Field(I);
            end loop;
            Get_Handle.Cursor_Field := Afpx_Xref.Records.Rate_001;
            Encode_Duration (Person.Sampling_Delta);

          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Reset
                and then Get_Handle.Cursor_Field >=  Afpx_Xref.Records.Tz1
                and then Get_Handle.Cursor_Field <= Afpx_Xref.Records.Tz6 then
            -- Reset of Tz from Person (if any, if not => clear)
            Encode_Tz;
          end if; -- Ptg_Result.Field_No = valid or cancel

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
    Mesure.Samples := (others => Pers_Def.Bpm_Range'First);
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
    Encode (Person, Mesure);

    -- Tittle
    Afpx.Encode_Field (Afpx_Xref.Records.Title, (00,00), "Deletion");

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
    for I in Mesu_Def.Sample_Nb_Range loop
      Protect (Afpx_Xref.Records.Rate_001 + Afpx.Field_Range(I) - 1);
    end loop;

    -- Disable import
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Import, False);

    -- Disable Ins, Suppr, ClearAll and TZReset
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Ins, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Del, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Clear, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Reset, False);


    -- Loop of ptgs
    loop
      Afpx.Encode_Field (Afpx_Xref.Records.Date, (00, 00),
                         Str_Mng.Current_Date_Printed);
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);

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

          if Ptg_Result.Field_No = Afpx_Xref.Records.Cancel then
            -- Cancel
            File_Name := Mesu_Nam.File_Name_Str'((others => ' '));
            exit;
          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Valid then
            -- OK. The caller will have to delete the file
            --  (after unselecting it)
            exit;
          end if; -- Ptg_Result.Field_No = valid or cancel
      end case; -- Result.Event

    end loop;

  end Delete;

end Mesu_Edi;

