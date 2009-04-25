with Event_Mng;
separate (Replace_Pattern)

function Shell_Command (Cmd : String) return String is
  Code : Command.Exit_Code_Range;
begin
  if Debug.Set then
    Sys_Calls.Put_Line_Error ("Replace, launching command: >"
      & Cmd & "<");
  end if;

  -- Execute command and check exit code
  Command.Execute (Cmd, True, Command.Only_Out,
     Out_Flow'Access, Err_Flow'Access, Code);
  Event_Mng.Reset_Default_Signals_Policy;
  if Code /= 0 then
    Sys_Calls.Put_Line_Error ("Replace, command exited with code "
                            & Code_Image (Code));
    raise Command_Error;
  end if;
  if Debug.Set then
    Sys_Calls.Put_Line_Error ("Replace, command returned: >"
      & Asu.To_String (Out_Flow.Str) & "<");
  end if;

  -- Return result
  return Asu.To_String (Out_Flow.Str);
end Shell_Command;

