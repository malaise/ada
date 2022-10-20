with Many_Strings, Command, Int_Img;
separate (Replace_Pattern)

package body Shell_Command is
  Out_Flow : aliased Command.Flow_Rec (Command.Str);

  function Exec (Cmd : String) return String is
    Code : Command.Exit_Code_Range;
    function Code_Image (Code : Integer) return String renames Int_Img;
  begin
    Log.Rep ("Launching command: >" & Cmd & "<");

    -- Execute command and check exit code
    Command.Execute (Many_Strings.Set (Cmd), True, Out_Flow'Access, null, Code);
    if Code /= 0 then
      Put_Error ("Command exited with code " & Code_Image (Code));
      raise Command_Error;
    end if;
    Log.Rep ("Command returned: >" & Out_Flow.Str.Image & "<");

    -- Return result
    return Out_Flow.Str.Image;
  exception
    when Command.Spawn_Error =>
      Put_Error ("Command spawning failed");
      raise Command_Error;
    when Command.Terminate_Request =>
      raise Terminate_Request;
  end Exec;
end Shell_Command;

