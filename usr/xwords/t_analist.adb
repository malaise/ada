with As.U; use As.U;
with Basic_Proc, Argument;
with Analist;
procedure T_Analist is

begin
  for Arg in 1 .. Argument.Get_Nbre_Arg loop
    declare
      Letters : constant String := Argument.Get_Parameter (Arg);
    begin
      Basic_Proc.Put_Line_Output (Letters & " -->");
      Analist.List (Letters, Min => 5);
      for Len in reverse 1 .. Letters'Length loop
        for I in 1 .. Analist.Result(Len).Length loop
          Basic_Proc.Put_Line_Output (
              Asu_Ts (Analist.Result(Len).Element(I)));
        end loop;
      end loop;
    end;
  end loop;
end T_Analist;

