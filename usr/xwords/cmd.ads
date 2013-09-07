with Trace.Loggers, Command;
package Cmd is
  -- A common logger
  Logger : Trace.Loggers.Logger;

  package Res_Mng renames Command.Res_Mng;
  subtype Res_List is Res_Mng.Dyn_List.List_Type;

  -- Execute command
  -- Set result and its output/error data
  Terminate_Request : exception;
  procedure Exec (Com : in String;
                  Arg : in String;
                  Ok : out Boolean;
                  Res : in out Res_List);
end Cmd;

