with Sys_Calls;
procedure Clear_Screen is
  Command : constant String := "clear";
  Res : Integer;
begin
  Res := Sys_Calls.Call_System (Command);
end Clear_Screen;

