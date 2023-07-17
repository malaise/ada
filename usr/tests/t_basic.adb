-- Test some functions of Basic_Proc; fixed sequence
with Basic_Proc;
procedure T_Basic is
  Str : String(1 .. 255);
  Len : Natural;
begin
  loop
    Basic_Proc.Get_Line (Str, Len);
    Basic_Proc.Put_Line_Output (Str (1 .. Len));
    Basic_Proc.Put_Line_Output (Basic_Proc.Get_Line);
  end loop;
exception
  when Basic_Proc.End_Error =>
    Basic_Proc.Put_Line_Error ("End_Error");
end T_Basic;

