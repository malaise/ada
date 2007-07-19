with Sys_Calls, Argument, Text_Line;
procedure T_Text_Line is

  procedure Do_One (File_Name : in String) is
    Fd : Sys_Calls.File_Desc;
    In_File, Out_File : Text_Line.File_Type;
  begin
    if File_Name /= "" then
      Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
      Text_Line.Open (In_File, Text_Line.In_File, Fd);
    else
      Text_Line.Open (In_File, Text_Line.In_File, Sys_Calls.Stdin);
    end if;
    Text_Line.Open (Out_File, Text_Line.Out_File, Sys_Calls.Stdout);
    loop
      declare
        Str : constant String := Text_Line.Get (In_File);
      begin
        exit when Str = "";
        Text_Line.Put (Out_File, Str);
      end;
    end loop;
    Text_Line.Close (In_File);
    if File_Name /= "" then
      Sys_Calls.Close (Fd);
      Text_Line.Put_Line (Out_File, "<<EOF>>");
    end if;
    Text_Line.Close (Out_File);
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

