-- Reset and clear the terminal
with Sys_Calls, Console;
procedure Clear_Screen is
  Dummy_Res : Boolean;
begin
  Dummy_Res := Sys_Calls.Set_Tty_Attr (Sys_Calls.Stdin, Sys_Calls.Canonical);
  Console.Clear;
end Clear_Screen;

