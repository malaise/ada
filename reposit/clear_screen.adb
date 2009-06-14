with Sys_Calls, Console;
procedure Clear_Screen is
  Res : Boolean;
  pragma Unreferenced (Res);
begin
  Res := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, Sys_Calls.Canonical);
  Console.Clear;
end Clear_Screen;

