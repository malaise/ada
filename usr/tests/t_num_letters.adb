with Ada.Text_Io, Ada.Exceptions;
with Argument, Num_Letters;

procedure T_Num_Letters is

  N : Num_Letters.Number;
  Ok : Boolean;
begin
  for I in 1 .. Argument.Get_Nbre_Arg loop

    Ada.Text_Io.Put (Argument.Get_Parameter (Occurence => I) & " --> ");
    begin
      N := Num_Letters.Number'Value (Argument.Get_Parameter (Occurence => I));
      Ok := True;
    exception
      when Constraint_Error =>
        Ada.Text_Io.Put_Line ("invalid number");
        Ok := False;
    end;

    if Ok then
      begin
        Ada.Text_Io.Put_Line (Num_Letters.Letters_Of (N) & " <");
      exception
        when Error:others =>
          Ada.Text_Io.Put_Line ("exception "
            & Ada.Exceptions.Exception_Name (Error));
      end;
    end if;

  end loop;

end T_Num_Letters;

