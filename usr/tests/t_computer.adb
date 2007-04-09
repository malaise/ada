with Ada.Text_Io;
with Argument, Computer;
procedure T_Computer is
begin
  Ada.Text_Io.Put_Line (Integer'Image (
       Computer.Eval (Argument.Get_Parameter)));
end T_Computer;

