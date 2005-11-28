with Ada.Text_Io;
with Sys_Calls, Argument, Text_Line;
procedure T_Text_Line is

  procedure Do_One (File_Name : in String) is
    Fd : Sys_Calls.File_Desc;
    File : Text_Line.File_Type;
  begin
    Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
    Text_Line.Open (File, Fd);
    loop
      declare
        Str : constant String := Text_Line.Get (File);
      begin
        exit when Str = "";
        Ada.Text_Io.Put (Str);
      end;
    end loop;
    Ada.Text_Io.Put_Line ("<<EOF>>");
    Text_Line.Close (File);
    Sys_Calls.Close (Fd);
  exception
    when Sys_Calls.Name_Error =>
      Sys_Calls.Put_Line_Error ("Name error on " & File_Name & ".");
  end Do_One;

begin
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Do_One (Argument.Get_Parameter (Occurence => I));
  end loop;
end T_Text_Line;

