with Aski, Normal, Afpx, Sys_Calls, Text_Line;
with Str_Mng, Mesu_Fil, Pers_Def, Mesu_Def, Mesu_Nam, Pers_Mng, Afpx_Xref;
package body Mesu_Prt is

  Printer_Name : constant String := "";
  Printer_Command : constant String := "heart_print";
  Printer      : Text_Line.File_Type;

  procedure Print_Rec (Person : in Pers_Def.Person_Rec;
                       Mesure : in Mesu_Def.Mesure_Rec) is
    Last_Of_Line : Boolean;
    use Pers_Def;
  begin
    if not Printer.Is_Open then
      Printer.Create_All (Printer_Name);
    end if;
    Printer.Put_Line ("Person: " & Person.Name & "    " & Person.Activity
                      & "    Date: " & Str_Mng.To_Printed_Str(Mesure.Date));
    Printer.Put ("Comment: " & Mesure.Comment
                & "   Delta: " & Normal(Integer(Mesure.Sampling_Delta), 3)
                & "    TZ: ");
    for I in Pers_Def.Person_Tz_Array'Range loop
      Printer.Put (Str_Mng.To_Str(Mesure.Tz(I)) & " ");
    end loop;
    Printer.New_Line;
    Last_Of_Line := False;
    for I in Mesu_Def.Sample_Nb_Range loop
      exit when Mesure.Samples(I) = Pers_Def.Bpm_Range'First;
      Printer.Put (Str_Mng.To_Str(Mesure.Samples(I)));
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
    Saved_Pos : Natural;
    Line      : Afpx.Line_Rec;
    File_Name : Mesu_Nam.File_Name_Str;
    Date_S    : Mesu_Nam.File_Date_Str;
    No_S      : Mesu_Nam.File_No_Str;
    Pid_S     : Mesu_Nam.File_Pid_Str;
    Pos_Pers  : Natural;
    Person    : Pers_Def.Person_Rec;
    Mesure    : Mesu_Def.Mesure_Rec;
    Dummy     : Integer;

  begin
    -- List is not empty
    Saved_Pos := Afpx.Line_List.Get_Position;

    -- for each in list
    Afpx.Line_List.Rewind;

    Afpx.Use_Descriptor (Afpx_Xref.Printing.Dscr_Num);
    Afpx.Put;

    Print:
    loop
      -- Get line, file_name, split
      Afpx.Line_List.Read (Line, Afpx.Line_List_Mng.Current);
      Str_Mng.Format_List_To_Mesure (Line, File_Name);
      Mesu_Nam.Split_File_Name (File_Name, Date_S, No_S, Pid_S);
      -- Get person
      Pers_Mng.Search (Pers_Def.The_Persons, Pers_Def.Pid_Range'Value(Pid_S),
                       Pos_Pers);
      Pers_Def.The_Persons.Read (Person, Pers_Def.Person_List_Mng.Current);
      -- Get mesure
      Mesure := Mesu_Fil.Load (File_Name);

      Print_Rec (Person, Mesure);

      -- Next line except if list empty or end of list
      exit when Afpx.Line_List.Is_Empty
      or else not Afpx.Line_List.Check_Move;

      Afpx.Line_List.Move_To;
      Print_Separator;
    end loop Print;

    Close;

    -- Print
    Dummy := Sys_Calls.Call_System(Printer_Command & " " & Printer_Name);

    -- Restore pos
    Afpx.Line_List.Move_At (Saved_Pos);

  exception
    when others =>
      Afpx.Line_List.Move_At (Saved_Pos);
  end Print;

end Mesu_Prt;

