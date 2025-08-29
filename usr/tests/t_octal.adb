with Directory, Basic_Proc;
procedure T_Octal is
begin
  loop
    -- Read a line, convert it to octal sequence and back
    declare
      Line : constant String := Basic_Proc.Get_Line;
      Octal : constant String := Directory.To_Sequence (Line);
      Res : constant String := Directory.To_Bytes (Octal);
    begin
      Basic_Proc.Put_Line_Output (Line);
      Basic_Proc.Put_Line_Output (Octal);
      Basic_Proc.Put_Line_Output (Res);
      if Res /= Line then
        Basic_Proc.Set_Error_Exit_Code;
        exit;
      end if;
    end;
  end loop;
exception
  when Basic_Proc.End_Error =>
    -- End of input stream
    null;
end T_Octal;

