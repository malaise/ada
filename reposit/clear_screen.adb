with Sys_Calls;
procedure Clear_Screen is
  Command : constant String := "clear";
  Res : Boolean;
begin
  Res := Sys_Calls.Set_Stdin_Attr (Sys_Calls.Canonical);
  Res := Sys_Calls.Call_System (Command) = 0;
end Clear_Screen;

