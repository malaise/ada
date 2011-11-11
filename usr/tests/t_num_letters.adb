with Ada.Exceptions;
with Basic_Proc, Argument, Num_Letters;

procedure T_Num_Letters is

  N : Num_Letters.Number;
  Ok : Boolean;
begin
  for I in 1 .. Argument.Get_Nbre_Arg loop

    Basic_Proc.Put_Output (Argument.Get_Parameter (Occurence => I) & " -->");
    begin
      N := Num_Letters.Number'Value (Argument.Get_Parameter (Occurence => I));
      Ok := True;
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output (" Invalid number");
        Ok := False;
    end;

    if Ok then
      begin
        Basic_Proc.Put_Line_Output (Num_Letters.Letters_Of (N) & "<");
      exception
        when Error:others =>
          Basic_Proc.Put_Line_Output (" Exception "
            & Ada.Exceptions.Exception_Name (Error));
      end;
    end if;

  end loop;

end T_Num_Letters;

