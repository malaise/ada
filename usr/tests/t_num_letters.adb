with Ada.Exceptions;
with Basic_Proc, Argument, Arbitrary, Num_Letters;

procedure T_Num_Letters is

  N : Num_Letters.Number;
  Scale : Num_Letters.Scale_List;
  Ok : Boolean;
begin
  -- default scale
  Scale := Num_Letters.Long;
  for I in 1 .. Argument.Get_Nbre_Arg loop

    declare
      Str : constant String := Argument.Get_Parameter (Occurence => I);
    begin
      if Str = "-l" then
        Scale := Num_Letters.Long;
        Ok := False;
      elsif Str = "-s" then
        -- Change scale for next num
        Scale := Num_Letters.Short;
        Ok := False;
      else
        N := Arbitrary.Set (Str);
        Basic_Proc.Put_Output (Str & " -->");
        Ok := True;
      end if;
    exception
      when Constraint_Error =>
        Basic_Proc.Put_Line_Output (" Invalid number");
        -- Back to default scale
        Ok := False;
    end;

    if Ok then
      begin
        Basic_Proc.Put_Line_Output (Num_Letters.Letters_Of (N, Scale) & "<");
        -- Back to default scale
        Scale := Num_Letters.Long;
      exception
        when Error:others =>
          Scale := Num_Letters.Long;
          Basic_Proc.Put_Line_Output (" Exception "
            & Ada.Exceptions.Exception_Name (Error));
      end;
    end if;

  end loop;

end T_Num_Letters;

