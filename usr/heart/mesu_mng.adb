with Afpx, Normal, Dir_Mng;
with Mesu_Edi, Pers_Mng, Mesu_Def, Mesu_Sel, Mesu_Nam, Pers_Lis, Mesu_Fil,
     Mesu_Prt, Mesu_Gra, Afpx_Xref;
package body Mesu_Mng is

  procedure List_Mesures (Nb_Month : in Str_Mng.Offset_Range) is
    Pers_Empty   : Boolean;
    List_Empty   : Boolean;
    Allow_Undo   : Boolean;
    Allow_Draw   : Boolean;
    Get_Handle   : Afpx.Get_Handle_Rec;
    Ptg_Result   : Afpx.Result_Rec;
    Ok           : Boolean;
    Criteria     : Mesu_Sel.Criteria_Rec;
    Line         : Afpx.Line_Rec;
    File_Name    : Mesu_Nam.File_Name_Str;
    Exit_Program : Boolean;
    Current_Date : Str_Mng.Date_Str_Rec;
    Person : Pers_Def.Person_Rec;
    Date_Aft, Date_Bef : Mesu_Def.Date_Str;

    -- Encode person namle and activity
    procedure Encode_Person is
    begin
      Afpx.Encode_Field (Afpx_Xref.Main.Person, (00, 00), Person.Name);
      Afpx.Encode_Field (Afpx_Xref.Main.Activity, (00, 00), Person.Activity);
    end Encode_Person;
    -- Encode Date in Before fields
    procedure Encode_After is
      Rec : Str_Mng.Date_Str_Rec;
    begin
      Str_Mng.To_Rec (Date_Aft, Rec);
      Afpx.Encode_Field (Afpx_Xref.Main.Day_After, (00, 00), Rec.Day);
      Afpx.Encode_Field (Afpx_Xref.Main.Month_After, (00, 00), Rec.Month);
      Afpx.Encode_Field (Afpx_Xref.Main.Year_After, (00, 00), Rec.Year);
    end Encode_After;
    -- Encode Date in After fields
    procedure Encode_Before is
      Rec : Str_Mng.Date_Str_Rec;
    begin
      Str_Mng.To_Rec (Date_Bef, Rec);
      Afpx.Encode_Field (Afpx_Xref.Main.Day_Before, (00, 00), Rec.Day);
      Afpx.Encode_Field (Afpx_Xref.Main.Month_Before, (00, 00), Rec.Month);
      Afpx.Encode_Field (Afpx_Xref.Main.Year_Before, (00, 00), Rec.Year);
    end Encode_Before;

    -- Check a field
    procedure Check_Field (Current_Field : in out Afpx.Absolute_Field_Range;
                           For_Valid : in Boolean;
                           Ok : out Boolean) is
      Locok : Boolean;
      Date_Aft_R, Date_Bef_R : Str_Mng.Date_Str_Rec;
      Pos_Pers : Integer;
      use type Afpx.Absolute_Field_Range;
    begin
      case Current_Field is

        when Afpx_Xref.Main.Person | Afpx_Xref.Main.Activity =>
          -- In name or activity
          -- Expand name & activity
          Person.Name     := Afpx.Decode_Field (Afpx_Xref.Main.Person, 00);
          Person.Activity := Afpx.Decode_Field (Afpx_Xref.Main.Activity, 00);

          if Str_Mng.Is_Spaces (Person.Name) then
            if Str_Mng.Is_Spaces (Person.Activity) then
              -- Name & activity empty : ok
              Current_Field := Afpx_Xref.Main.Day_After;
              Locok := True;
              if For_Valid then
                Criteria.Name := (others => ' ');
                Criteria.Activity := (others => ' ');
              end if;
            else
              -- Name emtpy but activity set : err name
              Current_Field := Afpx_Xref.Main.Person;
              Locok := False;
            end if;
          else
            Pers_Mng.Expand (Pers_Def.The_Persons,
                             Person.Name, Person.Activity, Pos_Pers);
            Encode_Person;

            -- Set pos in case of end
            if For_Valid then
              if Pos_Pers >= 0 then
                -- Some person found : ok
                Current_Field := Afpx_Xref.Main.Day_After;
                Locok := True;
                Criteria.Name := Person.Name;
                Criteria.Activity := Person.Activity;
              else
                -- Error in name
                Current_Field := Afpx_Xref.Main.Person;
                Locok := False;
              end if;
            else
              -- not for valid
              if Pos_Pers > 0 then
                -- one person found
                Current_Field := Afpx_Xref.Main.Day_After;
                Locok := True;
              elsif Pos_Pers = 0 then
                -- Several persons found : next field
                if Current_Field = Afpx_Xref.Main.Person then
                  Current_Field := Afpx_Xref.Main.Activity;
                else
                  Current_Field := Afpx_Xref.Main.Day_After;
                end if;
                Locok := True;
              else
                -- Error in name
                Current_Field := Afpx_Xref.Main.Person;
                Locok := False;
              end if;
            end if;
          end if;


        when Afpx_Xref.Main.Day_After | Afpx_Xref.Main.Month_After
           | Afpx_Xref.Main.Year_After =>
          -- In date aft
          Date_Aft_R.Day   := Afpx.Decode_Field (Afpx_Xref.Main.Day_After,
                                                 00);
          Date_Aft_R.Month := Afpx.Decode_Field (Afpx_Xref.Main.Month_After,
                                                 00);
          Date_Aft_R.Year  := Afpx.Decode_Field (Afpx_Xref.Main.Year_After,
                                                 00);

          if       Str_Mng.Is_Spaces (Date_Aft_R.Day)
          and then Str_Mng.Is_Spaces (Date_Aft_R.Month)
          and then Str_Mng.Is_Spaces (Date_Aft_R.Year) then
            Date_Aft := (others => ' ');
            Current_Field := Afpx_Xref.Main.Day_Before;
            Locok := True;
          else
            Current_Field := Afpx_Xref.Main.Day_After;
            Str_Mng.Check_Date (Date_Aft_R, True, Date_Aft, Locok);
            if Locok then
              Current_Field := Afpx_Xref.Main.Day_Before;
              Str_Mng.To_Rec (Date_Aft, Date_Aft_R);
              Encode_After;
            end if;
          end if;
          if Locok and then For_Valid then
            Criteria.Date_Aft := Date_Aft;
          end if;

        when Afpx_Xref.Main.Day_Before | Afpx_Xref.Main.Month_Before
           | Afpx_Xref.Main.Year_Before =>
          -- In date bef
          Date_Bef_R.Day   := Afpx.Decode_Field (Afpx_Xref.Main.Day_Before,
                                                 00);
          Date_Bef_R.Month := Afpx.Decode_Field (Afpx_Xref.Main.Month_Before,
                                                 00);
          Date_Bef_R.Year  := Afpx.Decode_Field (Afpx_Xref.Main.Year_Before,
                                                 00);

          if       Str_Mng.Is_Spaces (Date_Bef_R.Day)
          and then Str_Mng.Is_Spaces (Date_Bef_R.Month)
          and then Str_Mng.Is_Spaces (Date_Bef_R.Year) then
            Date_Bef := (others => ' ');
            Current_Field := Afpx_Xref.Main.Person;
            Locok := True;
          else
            Current_Field := Afpx_Xref.Main.Day_Before;
            Str_Mng.Check_Date (Date_Bef_R, True, Date_Bef, Locok);
            if Locok then
              Current_Field := Afpx_Xref.Main.Person;
              Str_Mng.To_Rec (Date_Bef, Date_Bef_R);
              Encode_Before;
            end if;
          end if;
          if Locok and then For_Valid then
            Criteria.Date_Bef := Date_Bef;
          end if;

        when others =>
          null;
      end case;

      Get_Handle.Cursor_Col := 0;
      Get_Handle.Insert := False;
      Ok := Locok;

    end Check_Field;

    -- Save and restore citeria and list when calling aother screen
    Position : Afpx.Line_List_Mng.Ll_Natural := 0;
    procedure Save is
      Field : Afpx.Absolute_Field_Range;
      Ok : Boolean;
    begin
       -- Store content of fields
       Field := Afpx_Xref.Main.Activity;
       Check_Field (Field, True, Ok);
       Field := Afpx_Xref.Main.Day_After;
       Check_Field (Field, True, Ok);
       Field := Afpx_Xref.Main.Day_Before;
       Check_Field (Field, True, Ok);
       -- Backup list and position
       Mesu_Sel.Save;
       if Afpx.Line_List.Is_Empty then
         Position := 0;
       else
         Position := Afpx.Line_List.Get_Position;
       end if;
    end Save;
    procedure Restore is
      use type Afpx.Line_List_Mng.Ll_Natural;
    begin
      -- Encode fields
      Encode_Person;
      Encode_After;
      Encode_Before;
      -- Restore list and position
      Mesu_Sel.Load;
      if Position /= 0 then
        Afpx.Line_List.Move_At (Position);
        Afpx.Update_List (Afpx.Center_Selected);
      end if;
    end Restore;

    -- Init screen, reset or restore
    procedure Init (Reset : in Boolean) is
      Dummy_Res : Boolean;
    begin
      Afpx.Use_Descriptor(Afpx_Xref.Main.Dscr_Num);
      Get_Handle.Cursor_Field := Afpx_Xref.Main.Person;
      Get_Handle.Cursor_Col := 0;
      Get_Handle.Insert := False;
      Allow_Undo := False;
      if Reset then
        Mesu_Sel.Load;
        Afpx.Update_List (Afpx.Center_Selected);
        if Nb_Month /= 0 then
          Str_Mng.Current_Date_Rec (Current_Date, Nb_Month);
          if Afpx.Line_List.Is_Empty then
            -- List empty : Set Aft to current date - offset
            Str_Mng.Check_Date (Current_Date, True, Date_Aft, Dummy_Res);
            Encode_After;
          else
            -- List not empty : Set Bef to current date - offset
            Str_Mng.Check_Date (Current_Date, False, Date_Bef, Dummy_Res);
            Encode_Before;
          end if;
        end if;
      else
        -- Not reset
        Restore;
      end if;
    end Init;

    use type Afpx.Absolute_Field_Range;
  begin
    Init (True);

    loop

      Pers_Empty := Pers_Def.The_Persons.Is_Empty;
      List_Empty := Afpx.Line_List.Is_Empty;
      Allow_Draw := not Pers_Empty and then
                    not List_Empty and then
                    Natural (Afpx.Line_List.List_Length)
                        <= Mesu_Gra.Max_Nb_Mesure;
      Allow_Undo := Allow_Undo and then not Pers_Empty;
      -- Tittle and slashes
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Title, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Slash1_Da, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Slash2_Da, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Slash1_Db, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Slash2_Db, not Pers_Empty);
      -- Pers, date
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Person, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Activity, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Day_After, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Month_After, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Year_After, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Day_Before, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Month_Before, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Year_Before, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Add, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Remove, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Undo, Allow_Undo);
      -- Buttons
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Unselect,
                                 not Pers_Empty and then not List_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Draw, Allow_Draw);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Print,
                                 not Pers_Empty and then not List_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Create, not Pers_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Clone,
                                 not Pers_Empty and then not List_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Edit,
                                 not Pers_Empty and then not List_Empty);
      Afpx.Set_Field_Activation (Afpx_Xref.Main.Delete,
                                 not Pers_Empty and then not List_Empty);

      Afpx.Encode_Field (Afpx_Xref.Main.Nb_Selected, (0, 0),
                         Normal (Natural (Afpx.Line_List.List_Length), 5) );

      Afpx.Encode_Field (Afpx_Xref.Main.Date, (00, 00),
                         Str_Mng.Current_Date_Printed);
      Afpx.Put_Then_Get (Get_Handle, Ptg_Result);

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
              if Get_Handle.Cursor_Field = Afpx_Xref.Main.Person  then
                Afpx.Clear_Field (Afpx_Xref.Main.Person);
                Afpx.Clear_Field (Afpx_Xref.Main.Activity);
                Get_Handle.Cursor_Field := Afpx_Xref.Main.Person;
              elsif Get_Handle.Cursor_Field = Afpx_Xref.Main.Activity then
                Afpx.Clear_Field (Afpx_Xref.Main.Activity);
              elsif Get_Handle.Cursor_Field = Afpx_Xref.Main.Day_After
              or else Get_Handle.Cursor_Field = Afpx_Xref.Main.Month_After
              or else Get_Handle.Cursor_Field = Afpx_Xref.Main.Year_After then
                Afpx.Clear_Field (Afpx_Xref.Main.Day_After);
                Afpx.Clear_Field (Afpx_Xref.Main.Month_After);
                Afpx.Clear_Field (Afpx_Xref.Main.Year_After);
                Get_Handle.Cursor_Field := Afpx_Xref.Main.Day_After;
              elsif Get_Handle.Cursor_Field = Afpx_Xref.Main.Day_Before
              or else Get_Handle.Cursor_Field = Afpx_Xref.Main.Month_Before
              or else Get_Handle.Cursor_Field = Afpx_Xref.Main.Year_Before then
                Afpx.Clear_Field (Afpx_Xref.Main.Day_Before);
                Afpx.Clear_Field (Afpx_Xref.Main.Month_Before);
                Afpx.Clear_Field (Afpx_Xref.Main.Year_Before);
                Get_Handle.Cursor_Field := Afpx_Xref.Main.Day_Before;
              else
                Afpx.Clear_Field (Get_Handle.Cursor_Field);
              end if;
              Get_Handle.Cursor_Col := 0;
              Get_Handle.Insert := False;
            when Afpx.Break_Key =>
              exit;
          end case;

        when Afpx.Mouse_Button =>

          if Ptg_Result.Field_No = Afpx_Xref.Main.Add
          or else Ptg_Result.Field_No = Afpx_Xref.Main.Remove then
            -- Add/Rem selec : check all fields one by one
            Get_Handle.Cursor_Field := Afpx_Xref.Main.Person;
            loop
              Check_Field (Get_Handle.Cursor_Field, True, Ok);
              exit when not Ok
              or else Get_Handle.Cursor_Field = Afpx_Xref.Main.Person;
            end loop;
            if Ok then
              if Ptg_Result.Field_No = Afpx_Xref.Main.Add then
                Mesu_Sel.Add_Selection (Criteria);
              else
                Mesu_Sel.Rem_Selection (Criteria);
              end if;
              Mesu_Sel.Save;
              Allow_Undo := True;
            end if;
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Clear then
            -- Clear selec
            Mesu_Sel.Clear_Selection;
            Mesu_Sel.Save;
            Allow_Undo := True;
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Undo then
            -- Undo
            Mesu_Sel.Undo;
            Mesu_Sel.Save;
            Allow_Undo := False;

          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Last_Month then
            -- Set After to one month ago
            Str_Mng.Current_Date_Rec (Current_Date, 1);
            Str_Mng.Check_Date (Current_Date, True, Date_Aft, Ok);
            Encode_After;
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Clear_After then
            -- Clear After fields
            Afpx.Clear_Field (Afpx_Xref.Main.Day_After);
            Afpx.Clear_Field (Afpx_Xref.Main.Month_After);
            Afpx.Clear_Field (Afpx_Xref.Main.Year_After);
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Clear_Before then
            -- Clear Before fields
            Afpx.Clear_Field (Afpx_Xref.Main.Day_Before);
            Afpx.Clear_Field (Afpx_Xref.Main.Month_Before);
            Afpx.Clear_Field (Afpx_Xref.Main.Year_Before);
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Db then
            -- Activiy Db
            Save;
            Pers_Lis.List (Exit_Program);
            if Exit_Program then
              exit;
            end if;
            Init (False);
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Unselect then
            -- Unselect
            Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
            Str_Mng.Format_List_To_Mesure (Line, File_Name);
            Mesu_Sel.Rem_Selection (File_Name);
            Mesu_Sel.Save;
            Allow_Undo := True;
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Draw then
            -- Draw
            Save;
            Mesu_Gra.Graphic;
            Init (False);
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Print then
            -- Print
            Save;
            Mesu_Prt.Print;
            Init (False);
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Create then
            -- Create
            File_Name := (others => ' ');
            Save;
            Mesu_Edi.Edit (File_Name);
            Init (False);
            if not Str_Mng.Is_Spaces (File_Name) then
              Mesu_Sel.Add_Selection (File_Name);
            end if;
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Clone then
            -- Clone
            Save;
            Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
            Str_Mng.Format_List_To_Mesure (Line, File_Name);
            Mesu_Edi.Clone (File_Name);
            Init (False);
            if not Str_Mng.Is_Spaces (File_Name) then
              Mesu_Sel.Add_Selection (File_Name);
            end if;
          elsif Ptg_Result.Field_No = 0
          or else Ptg_Result.Field_No = Afpx_Xref.Main.Edit then
            -- Edit
            Save;
            Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
            Str_Mng.Format_List_To_Mesure (Line, File_Name);
            Mesu_Edi.Edit (File_Name);
            Init (False);
            if not Str_Mng.Is_Spaces (File_Name) then
              Mesu_Sel.Rem_Selection (Line);
              Mesu_Sel.Add_Selection (File_Name);
            end if;
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Delete then
            -- Delete
            Save;
            Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
            Str_Mng.Format_List_To_Mesure (Line, File_Name);
            -- Screen to validate the deletion
            Mesu_Edi.Delete (File_Name);
            Init (False);
            if not Str_Mng.Is_Spaces (File_Name) then
              -- Validated: Unselect then remove mesu file
              Mesu_Sel.Rem_Selection (Line);
              Mesu_Fil.Delete (File_Name);
            end if;
          elsif Ptg_Result.Field_No = Afpx_Xref.Main.Quit then
            -- Done
            exit;
          end if; -- Test of buttons
      end case;

    end loop;

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

    if The_Files.Is_Empty then
      -- No file
      return;
    end if;
    Mesu_Sel.Load;

    -- Remove entries from selection, then files
    The_Files.Rewind;
    loop
      The_Files.Read (File, Dir_Mng.File_List_Mng.Current);
      File_Name := File.Name.Image;
      begin
        Mesu_Sel.Rem_Selection (File_Name);
      exception
        when Afpx.Line_List_Mng.Not_In_List =>
          -- This file was not selected
          null;
      end;
      Mesu_Fil.Delete (File_Name);
      -- Next file
      exit when not The_Files.Check_Move;
      The_Files.Move_To;
    end loop;

    The_Files.Delete_List;
    Mesu_Sel.Save;
  end Delete_All;

end Mesu_Mng;

