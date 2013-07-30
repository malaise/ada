with Con_Io, Afpx, Normal, Get_Line;
with As.U.Utils;
with Pers_Def, Pers_Mng, Mesu_Def, Mesu_Fil, Str_Mng, Afpx_Xref;
use Afpx;
-- with Mesu_Nam;
-- Edition, Creation, deletion of mesure
package body Mesu_Edi is

  subtype Import_File_Name_Str is String (1 ..22);

  procedure Encode (Person : in Pers_Def.Person_Rec;
                    Mesure : in Mesu_Def.Mesure_Rec) is
    Date_R : Str_Mng.Date_Str_Rec;
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

    Str_Mng.To_Rec (Mesure.Date, Date_R);
    Afpx.Encode_Field (Afpx_Xref.Records.Day, (00, 00), Date_R.Day);
    Afpx.Encode_Field (Afpx_Xref.Records.Month, (00, 00), Date_R.Month);
    Afpx.Encode_Field (Afpx_Xref.Records.Year, (00, 00), Date_R.Year);
    Afpx.Encode_Field (Afpx_Xref.Records.Comment, (00, 00), Mesure.Comment);
    Afpx.Encode_Field (Afpx_Xref.Records.Sampling, (00, 00),
                       Normal(Integer(Mesure.Sampling_Delta), 3));
    -- Samples
    for I in Mesu_Def.Sample_Nb_Range loop
      Afpx.Encode_Field (Afpx_Xref.Records.Rate_001 + Afpx.Field_Range(I) - 1,
                         (00, 00),
                         Str_Mng.To_Str(Mesure.Samples(I)) );
    end loop;
  end Encode;

  procedure Protect (Field_No : in Afpx.Field_Range) is
  begin
    Afpx.Set_Field_Colors (Field_No, Foreground => Con_Io.Color_Of ("Cyan"),
                                     Background => Con_Io.Color_Of ("Black"));
    Afpx.Set_Field_Protection (Field_No, True);
  end Protect;


  -- Import sample data from ascii file
  procedure Import_Samples (Import_File_Name : in Import_File_Name_Str;
                            Mesure : in out Mesu_Def.Mesure_Rec;
                            Ok : out Boolean) is
    Import_File_Name_Def : constant Import_File_Name_Str := (others => ' ');
    subtype Import_File_Name_Index is Integer range Import_File_Name_Str'Range;
    Import_File_Name_Last : Import_File_Name_Index;
    Samples : Mesu_Def.Max_Sample_Array
            := (others => Pers_Def.Bpm_Range'First);
    Samples_Index : Mesu_Def.Sample_Nb_Range := Mesu_Def.Sample_Nb_Range'First;
    package Get_Sample is new Get_Line (Comment => "#");
    Sample_Line : As.U.Utils.Asu_Ua.Unbounded_Array;
  begin
    Ok := False;
    -- Check if file name is empty
    if Import_File_Name = Import_File_Name_Def then
      return;
    end if;
    -- Check no ':' (file has to be on current drive)
    for I in Import_File_Name'Range loop
      if Import_File_Name(I) = ':' then
        return;
      end if;
    end loop;
    -- Remove trailing spaces
    for I in reverse Import_File_Name'Range loop
      if Import_File_Name(I) /= ' ' then
        -- Always occures cause Import_File_Name /= Import_File_Name_Def
        Import_File_Name_Last := I;
        exit;
      end if;
    end loop;

    -- Open file
    Get_Sample.Open(File_Name => Import_File_Name(Import_File_Name'First .. Import_File_Name_Last));

    -- Read, decode, store in Samples
    loop
      -- Split line in words
      Get_Sample.Get_Words (Sample_Line);
      -- Not empty nor a comment
      for I in 1 .. Get_Sample.Get_Word_Number loop
        -- Decode a Bpm
        Samples(Samples_Index) := Pers_Def.Bpm_Range'Value (
                                     Sample_Line.Element(I).Image);
        Samples_Index := Samples_Index + 1;
      end loop;

      -- Read next line and exit when end of file
      begin
        Get_Sample.Read_Next_Line;
      exception
        when Get_Sample.End_Error =>
          exit;
      end;
    end loop;

    -- Close file
    Get_Sample.Close;

    -- So far so good... Copy Samples in mesure
    Mesure.Samples := Samples;

    -- Done
    Ok := True;
  exception
    when others =>
      -- Ok is False. Close file if open.
      begin
        Get_Sample.Close;
      exception
        when others =>
          null;
      end;
  end Import_Samples;


  -- Edit a mesure.
  -- If date or person changes, then the file name may be affected.
  -- If File_Name is empty as input, then it is a creation and file_name
  --  is affected
  procedure Edit (File_Name : in out Mesu_Nam.File_Name_Str;
                  Exit_Program : out Boolean) is
    In_Create : Boolean;
    Person : Pers_Def.Person_Rec;
    Pos_Pers : Integer;
    Date_S : Mesu_Nam.File_Date_Str;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;
    Bpm_S  : Str_Mng.Bpm_Str;
    Mesure : Mesu_Def.Mesure_Rec;
    Valid_Date : Boolean;
    Space_Found : Boolean;

    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Ptg_Result   : Afpx.Result_Rec;

    Ok : Boolean;

    use Pers_Def;

    -- Encode TZ afpx fields from Person
    procedure Encode_Tz is
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
      Import_File_Name : Import_File_Name_Str;
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
              -- Encode Tz
              Encode_Tz;
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
            Str_Mng.Check_Date (Date_R, True, Mesure.Date, Valid_Date);
            Locok := Valid_Date;
          end if;
          if Locok then
            Current_Field := Afpx_Xref.Records.Comment;
          end if;

        when Afpx_Xref.Records.Comment =>
          -- In comment, no check
          Mesure.Comment := Afpx.Decode_Field (Afpx_Xref.Records.Comment, 00);
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
              Mesure.Sampling_Delta :=
                 Mesu_Def.Sampling_Delta_Range'Value(Delta_Str);
            exception
              when others =>
                Locok := False;
            end;
          end if;
          if Locok then
            Current_Field := Afpx_Xref.Records.Rate_001;
          end if;

        when Afpx_Xref.Records.Rate_001 .. Afpx_Xref.Records.Rate_100 =>
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
            if Current_Field /= Afpx_Xref.Records.Rate_100 then
              Current_Field := Current_Field + 1;
            else
              Current_Field := Afpx_Xref.Records.Person;
            end if;
          end if;

        when Afpx_Xref.Records.Import_File =>
          -- In import file name
          Import_File_Name := Afpx.Decode_Field (Current_Field, 00);
          -- Import data
          Import_Samples (Import_File_Name, Mesure, Locok);
          -- If ok, Encode samples and move to first sample,
          --  otherwise stay here
          if Locok then
            for I in Mesu_Def.Sample_Nb_Range loop
              Afpx.Encode_Field (
                  Afpx_Xref.Records.Rate_001 + Afpx.Field_Range(I) - 1,
                  (00, 00),
                  Str_Mng.To_Str(Mesure.Samples(I)) );
            end loop;
            Current_Field := Afpx_Xref.Records.Rate_001;
          end if;

        when others =>
          null;

      end case;

      Cursor_Col := 0;
      Insert := False;
      Ok := Locok;

    end Check_Field;


  begin
    -- Use descriptor
    Afpx.Use_Descriptor (Afpx_Xref.Records.Dscr_Num);
    In_Create := Str_Mng.Is_Spaces (File_Name);

    if In_Create then
      -- Set person
      Afpx.Encode_Field (Afpx_Xref.Records.Title, (00,00), "Creation");
      Person.Name := (others => ' ');
      Person.Activity := (others => ' ');
    else
      -- Load person
      Afpx.Encode_Field (Afpx_Xref.Records.Title, (00,00), " Edition");
      Mesu_Nam.Split_File_Name (File_Name, Date_S, No_S, Pid_S);
      Person.Pid := Pers_Def.Pid_Range'Value(Pid_S);
      Pers_Mng.Search (Pers_Def.The_Persons, Person.Pid, Pos_Pers);
      Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
      -- Disable import button
      Afpx.Set_Field_Activation (Afpx_Xref.Records.Import, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Records.Import_Title, False);
      Afpx.Set_Field_Activation (Afpx_Xref.Records.Import_File, False);
    end if;


    -- Set mesure
    if In_Create then
      -- Init date to current
      Mesure.Date := Str_Mng.Current_Date;
    else
      -- Load
      Mesure := Mesu_Fil.Load (File_Name);
    end if;


    Encode (Person, Mesure);

    if In_Create then
      -- Person name
      Cursor_Field := Afpx_Xref.Records.Person;
    else
      -- Date
      Cursor_Field := Afpx_Xref.Records.Day;
    end if;
    Cursor_Col := 0;
    Insert := False;

    -- Loop of Ptgs
    loop
      Afpx.Encode_Field (Afpx_Xref.Records.Date, (00, 00),
                         Str_Mng.Current_Date_Printed);
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result);

      case Ptg_Result.Event is
        when Fd_Event | Timer_Event | Signal_Event | Refresh =>
          null;
        when Keyboard =>

          case Ptg_Result.Keyboard_Key is
            when Return_Key =>
              -- Check field and go to next if Ok
              Check_Field (Cursor_Field, False, Ok);
            when Escape_Key =>
              -- Clear current field
              if Cursor_Field = Afpx_Xref.Records.Person then
                Afpx.Clear_Field (Afpx_Xref.Records.Person);
                Afpx.Clear_Field (Afpx_Xref.Records.Activity);
                Cursor_Field := Afpx_Xref.Records.Person;
              elsif Cursor_Field = Afpx_Xref.Records.Activity then
                Afpx.Clear_Field (Afpx_Xref.Records.Activity);
              elsif Cursor_Field = Afpx_Xref.Records.Day
              or else Cursor_Field = Afpx_Xref.Records.Month
              or else Cursor_Field = Afpx_Xref.Records.Year then
                Afpx.Clear_Field (Afpx_Xref.Records.Day);
                Afpx.Clear_Field (Afpx_Xref.Records.Month);
                Afpx.Clear_Field (Afpx_Xref.Records.Year);
                Cursor_Field := Afpx_Xref.Records.Day;
              else
                Afpx.Clear_Field (Cursor_Field);
              end if;
              Cursor_Col := 0;
              Insert := False;
            when Break_Key =>
              Exit_Program := True;
              exit;
          end case;

        when Mouse_Button =>
          if Ptg_Result.Field_No = Afpx_Xref.Records.Quit then
            -- Exit
            Exit_Program := True;
            exit;
          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Import then
            -- Import samples
            Cursor_Field := Afpx_Xref.Records.Import_File;
            Check_Field (Cursor_Field, False, Ok);
          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Cancel then
            -- Cancel
            File_Name := Mesu_Nam.File_Name_Str'((others => ' '));
            Exit_Program := False;
            exit;
          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Valid then
            -- Valid: Clear import file name
            Afpx.Clear_Field (Afpx_Xref.Records.Import_Title);

            -- Valid check all fields but import_file one by one
            Cursor_Field := Afpx_Xref.Records.Person;
            loop
              Check_Field (Cursor_Field, True, Ok);
              exit when not Ok or else Cursor_Field = Afpx_Xref.Records.Person;
            end loop;

            -- Check no hole in Tz, crescent
            if Ok then
              for I in Pers_Def.Person_Tz_Array'Range loop
                Cursor_Field := Afpx_Xref.Records.Tz1 + Afpx.Field_Range(I) - 1;
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
                Cursor_Field := Afpx_Xref.Records.Rate_001
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
              if not In_Create and then
                (        Mesure.Date /= Date_S
                 or else Pid_S /= Str_Mng.Pid_Str(Mesure.Pid)) then
                -- Necessity to change file_name. Delete previous.
                Mesu_Fil.Delete (File_Name);
              end if;
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
              Exit_Program := False;
              exit;
            end if;

          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Ins
                and then Cursor_Field >= Afpx_Xref.Records.Rate_001
                and then Cursor_Field <= Afpx_Xref.Records.Rate_100 then
            -- Insert a sample
            for I in reverse Cursor_Field + 1 .. Afpx_Xref.Records.Rate_100 loop
              Afpx.Encode_Field (I, (0, 0),
                 Afpx.Unicode_Sequence'(Afpx.Decode_Field(I - 1, 0)));
            end loop;
            Afpx.Clear_Field(Cursor_Field);

          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Del
                and then Cursor_Field >= Afpx_Xref.Records.Rate_001
                and then Cursor_Field <= Afpx_Xref.Records.Rate_100 then
            -- Suppress a sample
            for I in Cursor_Field .. Afpx_Xref.Records.Rate_100 - 1 loop
              Afpx.Encode_Field (I, (0, 0),
                 Afpx.Unicode_Sequence'(Afpx.Decode_Field(I + 1, 0)));
            end loop;
            Afpx.Clear_Field(Afpx_Xref.Records.Rate_100);

          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Reset
                and then Cursor_Field >=  Afpx_Xref.Records.Tz1
                and then Cursor_Field <= Afpx_Xref.Records.Tz6 then
            -- Reset of Tz from Person (if any, if not => clear)
            Encode_Tz;
          end if; -- Ptg_Result.Field_No = valid or cancel

      end case; -- Result.Event

    end loop;

  end Edit;

  -- Delete a mesure
  procedure Delete (File_Name : in out Mesu_Nam.File_Name_Str;
                    Exit_Program : out Boolean) is
    Person : Pers_Def.Person_Rec;
    Pos_Pers : Integer;
    Date_S : Mesu_Nam.File_Date_Str;
    No_S   : Mesu_Nam.File_No_Str;
    Pid_S  : Mesu_Nam.File_Pid_Str;
    Mesure : Mesu_Def.Mesure_Rec;

    Cursor_Field : Afpx.Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Insert       : Boolean;
    Ptg_Result   : Afpx.Result_Rec;
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
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Import_Title, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Import_File, False);

    -- Disable Ins, Suppr and TZReset
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Ins, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Del, False);
    Afpx.Set_Field_Activation (Afpx_Xref.Records.Reset, False);

    Cursor_Field := 01;
    Cursor_Col := 0;
    Insert := False;

    -- Loop of ptgs
    loop
      Afpx.Encode_Field (Afpx_Xref.Records.Date, (00, 00),
                         Str_Mng.Current_Date_Printed);
      Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Insert, Ptg_Result);

      case Ptg_Result.Event is
        when Fd_Event | Timer_Event | Signal_Event | Refresh =>
          null;
        when Keyboard =>

          case Ptg_Result.Keyboard_Key is
            when Return_Key | Escape_Key =>
              null;
            when Break_Key =>
              Exit_Program := True;
              exit;
          end case;

        when Mouse_Button =>

          if Ptg_Result.Field_No = Afpx_Xref.Records.Cancel then
            -- Cancel
            File_Name := Mesu_Nam.File_Name_Str'((others => ' '));
            Exit_Program := False;
            exit;
          elsif Ptg_Result.Field_No = Afpx_Xref.Records.Valid then
            -- Delete
            Mesu_Fil.Delete (File_Name);
            Exit_Program := False;
            exit;
          end if; -- Ptg_Result.Field_No = valid or cancel

      end case; -- Result.Event

    end loop;

  end Delete;

end Mesu_Edi;

