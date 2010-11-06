with As.U; use As.U;
with Basic_Proc, Argument;
with Analist;
procedure T_Analist is

  Result : Asu_Ua.Unb_Array;

begin
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Analist.List (Argument.Get_Parameter (I), Result);
    Basic_Proc.Put_Line_Output (Argument.Get_Parameter (Occurence => I)
                                & " -->");
    for J in 1 .. Result.Length loop
      Basic_Proc.Put_Line_Output (Asu_Ts (Result.Element (J)));
    end loop;
  end loop;
end T_Analist;

