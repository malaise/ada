with Con_Io, Afpx, Normal, Perpet, Dir_Mng;
with Mesu_Edi, Pers_Mng, Mesu_Def, Mesu_Sel, Mesu_Nam, Pers_Lis, Mesu_Fil,
     Pers_Def, Mesu_Prt, Mesu_Gra;
use Afpx;
package body Mesu_Mng is

  procedure List_Mesures (Nb_Month : in Str_Mng.Offset_Range) is
    Pers_Empty   : Boolean;
    List_Empty   : Boolean;
    Allow_Undo   : Boolean;
    Allow_Draw   : Boolean;
    Cursor_Field : Afpx.Absolute_Field_Range;
    Cursor_Col   : Con_Io.Col_Range;
    Ptg_Result   : Afpx.Result_Rec;
    Ok           : Boolean;
    Criteria     : Mesu_Sel.Criteria_Rec;
    Line         : Afpx.Line_Rec;
    File_Name    : Mesu_Nam.File_Name_Str;
    P_File_Name  : Mesu_Nam.File_Name_Str;
    Exit_Program : Boolean;
    Current_Date : Str_Mng.Date_Str_Rec;
    Redisplay    : Boolean;


    -- Check a field
    procedure Check_Field (Current_Field : in out Afpx.Absolute_Field_Range;
                           For_Valid : in Boolean;
                           Ok : out Boolean) is
      Locok : Boolean;
      Date_Aft_R, Date_Bef_R : Str_Mng.Date_Str_Rec;
      Pos_Pers : Integer;
      Person : Pers_Def.Person_Rec;
      Date_Aft, Date_Bef : Mesu_Def.Date_Str;
    begin
      case Current_Field is

        when 07 | 08 =>
          -- In name or activity
          -- Expand name & activity
          Person.Name     := Afpx.Decode_Field (07, 00);
          Person.Activity := Afpx.Decode_Field (08, 00);

          if Str_Mng.Is_Spaces (Person.Name) then
            if Str_Mng.Is_Spaces (Person.Activity) then
              -- Name & activity empty : ok
              Current_Field := 09;
              Locok := True;
              if For_Valid then
                Criteria.Name := (others => ' ');
                Criteria.Activity := (others => ' ');
              end if;
            else
              -- Name emtpy but activity set : err name
              Current_Field := 07;
              Locok := False;
            end if;
          else
            Pers_Mng.Expand (Pers_Def.The_Persons,
                             Person.Name, Person.Activity, Pos_Pers);
            Afpx.Encode_Field (07, (00, 00), Person.Name);
            Afpx.Encode_Field (08, (00, 00), Person.Activity);

            -- Set pos in case of end
            if For_Valid then
              if Pos_Pers >= 0 then
                -- Some person found : ok
                Current_Field := 09;
                Locok := True;
                Criteria.Name := Person.Name;
                Criteria.Activity := Person.Activity;
              else
                -- Error in name
                Current_Field := 07;
                Locok := False;
              end if;
            else
              -- not for valid
              if Pos_Pers > 0 then
                -- one person found
                Current_Field := 09;
                Locok := True;
              elsif Pos_Pers = 0 then
                -- Several persons found : next field
                if Current_Field = 07 then
                  Current_Field := 08;
                else
                  Current_Field := 09;
                end if;
                Locok := True;
              else
                -- Error in name
                Current_Field := 07;
                Locok := False;
              end if;
            end if;
          end if;


        when 09 | 10 | 11 =>
          -- In date aft
          Date_Aft_R.Day   := Afpx.Decode_Field (09, 00);
          Date_Aft_R.Month := Afpx.Decode_Field (10, 00);
          Date_Aft_R.Year  := Afpx.Decode_Field (11, 00);

          if       Str_Mng.Is_Spaces (Date_Aft_R.Day)
          and then Str_Mng.Is_Spaces (Date_Aft_R.Month)
          and then Str_Mng.Is_Spaces (Date_Aft_R.Year) then
            Date_Aft := (others => ' ');
            Current_Field := 12;
            Locok := True;
          else
            Current_Field := 09;
            Str_Mng.Check_Date (Date_Aft_R, True, Date_Aft, Locok);
            if Locok then
              Current_Field := 12;
              Str_Mng.To_Rec (Date_Aft, Date_Aft_R);
              Afpx.Encode_Field (09, (00, 00), Date_Aft_R.Day);
              Afpx.Encode_Field (10, (00, 00), Date_Aft_R.Month);
              Afpx.Encode_Field (11, (00, 00), Date_Aft_R.Year);
            end if;
          end if;
          if Locok and then For_Valid then
            Criteria.Date_Aft := Date_Aft;
          end if;

        when 12 | 13 | 14 =>
          -- In date bef
          Date_Bef_R.Day   := Afpx.Decode_Field (12, 00);
          Date_Bef_R.Month := Afpx.Decode_Field (13, 00);
          Date_Bef_R.Year  := Afpx.Decode_Field (14, 00);

          if       Str_Mng.Is_Spaces (Date_Bef_R.Day)
          and then Str_Mng.Is_Spaces (Date_Bef_R.Month)
          and then Str_Mng.Is_Spaces (Date_Bef_R.Year) then
            Date_Bef := (others => ' ');
            Current_Field := 07;
            Locok := True;
          else
            Current_Field := 12;
            Str_Mng.Check_Date (Date_Bef_R, True, Date_Bef, Locok);
            if Locok then
              Current_Field := 07;
              Str_Mng.To_Rec (Date_Bef, Date_Bef_R);
              Afpx.Encode_Field (12, (00, 00), Date_Bef_R.Day);
              Afpx.Encode_Field (13, (00, 00), Date_Bef_R.Month);
              Afpx.Encode_Field (14, (00, 00), Date_Bef_R.Year);
            end if;
          end if;
          if Locok and then For_Valid then
            Criteria.Date_Bef := Date_Bef;
          end if;

        when others =>
          null;
      end case;

      Cursor_Col := 0;
      Ok := Locok;

    end Check_Field;

  begin
    Afpx.Use_Descriptor(1);
    Cursor_Field := 07;
    Cursor_Col := 0;
    Mesu_Sel.Load;
    Afpx.Update_List (Afpx.Center);

    if Nb_Month /= 0 then
      Str_Mng.Current_Date_Rec (Current_Date, Nb_Month);
      if Afpx.Line_List_Mng.List_Length(Afpx.Line_List) = 0 then
        -- List empty : Set Aft to current date - offset
        Afpx.Encode_Field (09, (00, 00), Current_Date.Day);
        Afpx.Encode_Field (10, (00, 00), Current_Date.Month);
        Afpx.Encode_Field (11, (00, 00), Current_Date.Year);
      else
        -- List not empty : Set Bef to current date - offset
        Afpx.Encode_Field (12, (00, 00), Current_Date.Day);
        Afpx.Encode_Field (13, (00, 00), Current_Date.Month);
        Afpx.Encode_Field (14, (00, 00), Current_Date.Year);
      end if;
    end if;



    List:
    loop
      Allow_Undo := False;
      Redisplay := True;

      Ptg:
      loop
        Pers_Empty := Pers_Def.Person_List_Mng.Is_Empty (Pers_Def.The_Persons);
        List_Empty := Afpx.Line_List_Mng.List_Length(Afpx.Line_List) = 0;
        Allow_Draw := not Pers_Empty and then
                      not List_Empty and then
                      Afpx.Line_List_Mng.List_Length(Afpx.Line_List)
                            <= Mesu_Gra.Max_Nb_Mesure;
        Allow_Undo := Allow_Undo and then not Pers_Empty;
        -- Tittles
        Afpx.Set_Field_Activation (01, not Pers_Empty);
        Afpx.Set_Field_Activation (03, not Pers_Empty);
        Afpx.Set_Field_Activation (04, not Pers_Empty);
        Afpx.Set_Field_Activation (05, not Pers_Empty);
        Afpx.Set_Field_Activation (06, not Pers_Empty);
        -- Pers, date
        Afpx.Set_Field_Activation (07, not Pers_Empty);
        Afpx.Set_Field_Activation (08, not Pers_Empty);
        Afpx.Set_Field_Activation (09, not Pers_Empty);
        Afpx.Set_Field_Activation (10, not Pers_Empty);
        Afpx.Set_Field_Activation (11, not Pers_Empty);
        Afpx.Set_Field_Activation (12, not Pers_Empty);
        Afpx.Set_Field_Activation (13, not Pers_Empty);
        Afpx.Set_Field_Activation (14, not Pers_Empty);
        Afpx.Set_Field_Activation (15, not Pers_Empty);
        Afpx.Set_Field_Activation (16, not Pers_Empty);
        Afpx.Set_Field_Activation (17, Allow_Undo);
        -- Buttons
        Afpx.Set_Field_Activation (22, not Pers_Empty and then not List_Empty);
        Afpx.Set_Field_Activation (23, Allow_Draw);
        Afpx.Set_Field_Activation (24, not Pers_Empty and then not List_Empty);
        Afpx.Set_Field_Activation (25, not Pers_Empty);
        Afpx.Set_Field_Activation (26, not Pers_Empty and then not List_Empty);
        Afpx.Set_Field_Activation (27, not Pers_Empty and then not List_Empty);

        Afpx.Encode_Field (20, (0, 0),
          Normal(Afpx.Line_List_Mng.List_Length(Afpx.Line_List), 5) );

        Afpx.Encode_Field (02, (00, 00), Str_Mng.Current_Date_Printed);
        Afpx.Put_Then_Get (Cursor_Field, Cursor_Col, Ptg_Result, Redisplay);
        Redisplay := False;

        case Ptg_Result.Event is
          when Refresh =>
            Redisplay := True;
          when Fd_Event | Afpx.Timer_Event | Afpx.Signal_Event =>
            null;
          when Keyboard =>

            case Ptg_Result.Keyboard_Key is
              when Return_Key =>
                -- Check field and go to next if Ok
                Check_Field (Cursor_Field, False, Ok);
              when Escape_Key =>
                -- Clear current field
                if Cursor_Field = 07  then
                  Afpx.Clear_Field (07);
                  Afpx.Clear_Field (08);
                  Cursor_Field := 07;
                elsif Cursor_Field = 08 then
                  Afpx.Clear_Field (08);
                elsif Cursor_Field = 09 or else Cursor_Field = 10
                or else Cursor_Field = 11 then
                  Afpx.Clear_Field (09);
                  Afpx.Clear_Field (10);
                  Afpx.Clear_Field (11);
                  Cursor_Field := 09;
                elsif Cursor_Field = 12 or else Cursor_Field = 13
                or else Cursor_Field = 14 then
                  Afpx.Clear_Field (12);
                  Afpx.Clear_Field (13);
                  Afpx.Clear_Field (14);
                  Cursor_Field := 12;
                else
                  Afpx.Clear_Field (Cursor_Field);
                end if;
                Cursor_Col := 0;
              when Break_Key =>
                exit List;
            end case;

          when Mouse_Button =>

            if Ptg_Result.Field_No = 15 or else Ptg_Result.Field_No = 16 then
              -- Add/Rem selec : check all fields one by one
              Cursor_Field := 07;
              loop
                Check_Field (Cursor_Field, True, Ok);
                exit when not Ok or else Cursor_Field = 07;
              end loop;
              if Ok then
                if Ptg_Result.Field_No = 15 then
                  Mesu_Sel.Add_Selection (Criteria);
                else
                  Mesu_Sel.Rem_Selection (Criteria);
                end if;
                Allow_Undo := True;
              end if;
            elsif Ptg_Result.Field_No = 17 then
              -- Undo
              Mesu_Sel.Undo;
              Allow_Undo := False;
            elsif Ptg_Result.Field_No = 18 then
              -- Activiy Db
              Mesu_Sel.Save;
              Pers_Lis.List (Exit_Program);
              Mesu_Sel.Load;
              Afpx.Update_List(Afpx.Center);
              if Exit_Program then
                exit List;
              end if;
              exit Ptg;
            elsif Ptg_Result.Field_No = 19 then
              -- Exit
              exit List;
            elsif Ptg_Result.Field_No = 22 then
              -- Unselect
              Afpx.Line_List_Mng.Read (Afpx.Line_List, Line,
                                       Afpx.Line_List_Mng.Current);
              Str_Mng.Format_List_To_Mesure (Line, File_Name);
              Mesu_Sel.Rem_Selection (File_Name);
              Allow_Undo := True;
            elsif Ptg_Result.Field_No = 23 then
              -- Draw
              Mesu_Gra.Graphic(Exit_Program);
              if Exit_Program then
                exit List;
              end if;
              exit Ptg;
            elsif Ptg_Result.Field_No = 24 then
              -- Print
              Mesu_Prt.Print;
              exit Ptg;
            elsif Ptg_Result.Field_No = 25 then
              -- Create
              File_Name := (others => ' ');
              Mesu_Edi.Edit (File_Name, Exit_Program);
              if Exit_Program then
                exit List;
              end if;
              if not Str_Mng.Is_Spaces (File_Name) then
                Mesu_Sel.Add_Selection (File_Name);
              end if;
              -- Edit screen called
              exit Ptg;
            elsif (Ptg_Result.Field_No = 0
                   or else Ptg_Result.Field_No = 26) then
              -- Edit
              Afpx.Line_List_Mng.Read (Afpx.Line_List, Line,
                                       Afpx.Line_List_Mng.Current);
              Str_Mng.Format_List_To_Mesure (Line, File_Name);

              -- Edit
              P_File_Name := File_Name;
              Mesu_Edi.Edit (File_Name, Exit_Program);
              if Exit_Program then
                exit List;
              end if;
              if not Str_Mng.Is_Spaces (File_Name) then
                Mesu_Sel.Rem_Selection (Line);
                Mesu_Sel.Add_Selection (File_Name);
              end if;
              -- Edit screen called
              exit Ptg;
            elsif Ptg_Result.Field_No = 27 then
              -- Delete
              Afpx.Line_List_Mng.Read (Afpx.Line_List, Line,
                                       Afpx.Line_List_Mng.Current);
              Str_Mng.Format_List_To_Mesure (Line, File_Name);

              -- Delete
              P_File_Name := File_Name;
              Mesu_Edi.Delete (File_Name, Exit_Program);

              if Exit_Program then
                exit List;
              end if;
              if not Str_Mng.Is_Spaces (File_Name) then
                Mesu_Sel.Rem_Selection (Line);
              end if;
              -- Edit screen called
              exit Ptg;
            end if; -- Test of buttons
        end case;

      end loop Ptg;
      -- Another screen called
      Afpx.Use_Descriptor(1);
    end loop List;

    Mesu_Sel.Save;
  end List_Mesures;

  procedure Delete_All (Person : in Pers_Def.Person_Rec) is
    File_Name : Mesu_Nam.File_Name_Str;
    The_Files : Dir_Mng.File_List_Mng.List_Type;
    File : Dir_Mng.File_Entry_Rec;
  begin
    -- Build ????????.<pid>
    File_Name := Mesu_Nam.Build_File_Name (Pid => Str_Mng.Pid_Str (Person.Pid));
    -- Set files lin ist
    Dir_Mng.List_Dir (The_Files, "", File_Name);

    if Dir_Mng.File_List_Mng.Is_Empty (The_Files) then
      -- No file
      return;
    end if;
    Mesu_Sel.Load;

    -- Remove entries from selection, then files
    Dir_Mng.File_List_Mng.Rewind (The_Files);
    loop
      Dir_Mng.File_List_Mng.Read (The_Files, File,
                                  Dir_Mng.File_List_Mng.Current);
      File_Name := File.Name(1 .. File.Len);
      begin
        Mesu_Sel.Rem_Selection (File_Name);
      exception
        when Afpx.Line_List_Mng.Not_In_List =>
          -- This file was not selected
          null;
      end;
      Mesu_Fil.Delete (File_Name);
      -- Next file
      exit when not Dir_Mng.File_List_Mng.Check_Move (The_Files);
      Dir_Mng.File_List_Mng.Move_To (The_Files);
    end loop;

    Dir_Mng.File_List_Mng.Delete_List (The_Files);
    Mesu_Sel.Save;
  end Delete_All;

end Mesu_Mng;

