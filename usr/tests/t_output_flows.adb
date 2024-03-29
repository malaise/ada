-- Output a fixed string on Stdout (define 2 ways), Async_Stderr and a file
with Basic_Proc, Many_Strings, Command, Output_Flows, Text_Line, Sys_Calls;
procedure T_Output_Flows is
  F1, F2, F3 : Output_Flows.Output_Flow;
  File_Name : constant String := "data/t_output_flows.dat";
  Str : constant String := "File OK";
  Cmd : Many_Strings.Many_String;
  Res_Out : aliased Command.Flow_Rec(Command.Str);
  Code : Command.Exit_Code_Range;
begin

  F1.Set (Output_Flows.Stdout_Name);
  F1.Put_Line ("Got stdout");

  F2.Set (Output_Flows.Async_Stderr_Name);
  F1.Put_Line ("Got async stderr", True);
  F2.Put_Line ("Stderr OK");
  F2.Flush;

  declare
    F4 : Output_Flows.Output_Flow;
  begin
    F4 := Output_Flows.Get (File_Name);
    F4.Put_Line (Str);
  end;
  Cmd.Set ("cat");
  Cmd.Cat (File_Name);
  Command.Execute (Cmd, True, Res_Out'Unrestricted_Access, null, Code);
  if Code /= 0 then
    Basic_Proc.Put_Line_Error ("cat " & File_Name & " has exited with code"
                             & Code'Img);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  if Res_Out.Str.Image /= Str & Text_Line.Line_Feed_Char then
    Basic_Proc.Put_Line_Error (File_Name & " contains: " & Res_Out.Str.Image);
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;
  F1.Put_Line ("File OK");
  Sys_Calls.Unlink (File_Name);

  F3 := Output_Flows.Get (Output_Flows.Stdout_Name);
  F3.Put_Line ("Test OK");
end T_Output_Flows;

