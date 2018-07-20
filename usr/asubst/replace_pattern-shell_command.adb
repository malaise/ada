with Many_Strings, Command, Images;
separate (Replace_Pattern)

package body Shell_Command is
  Out_Flow, Err_Flow : aliased Command.Flow_Rec (Command.Str);

  function Exec (Cmd : String) return String is
    Code : Command.Exit_Code_Range;
    function Code_Image is new Images.Int_Image (Command.Exit_Code_Range);
  begin
    Log.Rep ("Launching command: >" & Cmd & "<");

    -- Execute command and check exit code
    Command.Execute (Many_Strings.Set (Cmd), True, Command.Only_Out,
       Out_Flow'Access, Err_Flow'Access, Code);
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

