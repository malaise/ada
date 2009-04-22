with Ada.Strings.Unbounded;
package Command is

  -- Issue '/bin/sh "Cmd"' and set resulting exit code (-1 if abnormal)
  -- and set execution output flow, possibly mixed with error flow
  procedure Shell (Cmd : in String;
                   Mix_Error_Flow : in Boolean;
                   Exit_Code : out Integer;
                   Out_Flow : out Ada.Strings.Unbounded.Unbounded_String);

  -- Terminate requested by Control C
  Terminate_Request : exception;

  -- Could not spawn /bin/sh
  Spawn_Error : exception;
end Command;

