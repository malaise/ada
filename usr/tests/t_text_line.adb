with Sys_Calls, Argument, Text_Line;
procedure T_Text_Line is

  procedure Do_One (File_Name : in String) is
    In_Fd : Sys_Calls.File_Desc;
    In_File, Out_File : Text_Line.File_Type;
  begin
    if File_Name /= "" then
      In_Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
      In_File.Open (Text_Line.In_File, In_Fd);
    else
      In_File.Open (Text_Line.In_File, Sys_Calls.Stdin);
    end if;
    Out_File.Open (Text_Line.Out_File, Sys_Calls.Stdout);
    In_File.Set_Line_Feed (Text_Line.Line_Feed_Str & Text_Line.Line_Feed_Str);
    Out_File.Set_Line_Feed (Text_Line.Get_Line_Feed (In_File));
    loop
      declare
        Str : constant String := In_File.Get;
      begin
        exit when Str = "";
        Out_File.Put (Str);
      end;
    end loop;
    In_File.Close;
    if File_Name /= "" then
      Sys_Calls.Close (In_Fd);
      Out_File.Put_Line ("<<EOF>>");
    end if;
    Out_File.Close;
  exception
    when Sys_Calls.Name_Error =>
      Sys_Calls.Put_Line_Error ("Name error on " & File_Name & ".");
  end Do_One;

begin
  if Argument.Get_Nbre_Arg = 0 then
    Do_One ("");
  else
    for I in 1 .. Argument.Get_Nbre_Arg loop
      Do_One (Argument.Get_Parameter (Occurence => I));
    end loop;
  end if;
end T_Text_Line;

