with Aski, Normal, Afpx.Utils, Sys_Calls, Text_Line, As.U, Temp_File;
with Str_Mng, Mesu_Fil, Pers_Def, Mesu_Def, Mesu_Nam, Pers_Mng, Afpx_Xref;
package body Mesu_Prt is

  Printer_Command : constant String := "heart_print";
  Printer      : Text_Line.File_Type;

  procedure Print_Rec (Person : in Pers_Def.Person_Rec;
                       Mesure : in Mesu_Def.Mesure_Rec) is
    Last_Of_Line : Boolean;
  begin
    Printer.Put_Line ("Person: " & Person.Name & "    " & Person.Activity
                      & "    Date: " & Str_Mng.To_Printed_Date_Str(Mesure.Date)
                      & "  Time: " & Str_Mng.To_Printed_Time_Str(Mesure.Time));
    Printer.Put ("Comment: " & Mesure.Comment
                & "   Delta: " & Normal(Integer(Mesure.Sampling_Delta), 3)
                & "    TZ: ");
    for Tz of Mesure.Tz loop
      Printer.Put (Str_Mng.To_Str(Tz) & " ");
    end loop;
    Printer.New_Line;
    Last_Of_Line := False;
    for I in 1 .. Mesure.Samples.Length loop
      Printer.Put (Str_Mng.To_Str(Mesure.Samples.Element (I)));
      Last_Of_Line := I mod 20 = 0;
      if Last_Of_Line then
        -- After last of row
        Printer.New_Line;
      else
        Printer.Put (" ");
      end if;
    end loop;
    if not Last_Of_Line then
      Printer.New_Line;
    end if;
  end Print_Rec;

  procedure Print_Separator is
  begin
    Printer.Put_Line (
"- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -");
  end Print_Separator;

  procedure Close is
  begin
    Printer.Put (Aski.Ff & "");
    Printer.Close_All;
  end Close;

  procedure Print is
    Printer_Name : As.U.Asu_Us;
    Bkp_Ctx   : Afpx.Utils.Backup_Context;
    Line      : Afpx.Line_Rec;
    File_Name : Mesu_Nam.File_Name_Str;
    Date_S    : Mesu_Nam.File_Date_Str;
    Time_S    : Mesu_Nam.File_Time_Str;
    Pid_S     : Mesu_Nam.File_Pid_Str;
    Pos_Pers  : Natural;
    Person    : Pers_Def.Person_Rec;
    Mesure    : Mesu_Def.Mesure_Rec;
    Dummy     : Integer;

  begin
    Printer_Name.Set (Temp_File.Create (".", "prt"));
    Printer.Create_All (Printer_Name.Image);
    -- List is not empty
    Bkp_Ctx.Backup;

    -- for each in list
    Afpx.Line_List.Rewind;

    Afpx.Use_Descriptor (Afpx_Xref.Printing.Dscr_Num);
    Afpx.Put;

    Print:
    loop
      -- Get line, file_name, split
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
      Str_Mng.Format_List_To_Mesure (Line, File_Name);
      Mesu_Nam.Split_File_Name (File_Name, Date_S, Time_S, Pid_S);
      -- Get person
      Pers_Mng.Search (Pers_Def.The_Persons, Pers_Def.Pid_Range'Value(Pid_S),
                       Pos_Pers);
      Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
      -- Get mesure
      Mesure := Mesu_Fil.Load (File_Name);

      Print_Rec (Person, Mesure);

      -- Next line except if list empty or end of list
      exit Print when Afpx.Line_List.Is_Empty
      or else not Afpx.Line_List.Check_Move;

      Afpx.Line_List.Move_To;
      Print_Separator;
    end loop Print;

    Close;

    -- Print
    Dummy := Sys_Calls.Call_System(Printer_Command & " " & Printer_Name.Image);

    -- Restore pos
    Bkp_Ctx.Restore (False);

    Sys_Calls.Unlink (Printer_Name.Image);
  exception
    when others =>
      Bkp_Ctx.Restore (False);
  end Print;

end Mesu_Prt;

