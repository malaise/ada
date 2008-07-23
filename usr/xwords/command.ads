with Dynamic_List;
with Common;
use Common;
package Command is

  subtype Line_Type is Asu_Us;
  package Res_Mng is new Dynamic_List (Line_Type);
  subtype Res_List is Res_Mng.Dyn_List.List_Type;

  -- Execute command
  -- Set result and its output/error data
  Terminate_Request : exception;
  procedure Exec (Command : in String;
                  Argument : in String;
                  Ok : out Boolean;
                  Result : in out Res_List);
end Command;

