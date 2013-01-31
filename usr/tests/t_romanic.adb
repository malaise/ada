-- Test romanic 2 arabic conversion
-- With no arg, loops on all values (1 to 3999)
--  otherwise converts arguments
with Ada.Exceptions;
with Basic_Proc, Argument, Romanic;

procedure T_Romanic is
  Skip_It : exception;
begin

  if Argument.Get_Nbre_Arg = 0 then
    for I in Romanic.Arabic loop
      declare
        Nr : constant Romanic.Number := Romanic.Arabic2Romanic(I);
        Na : constant Romanic.Arabic := Romanic.Romanic2Arabic(Nr);
      begin
        Basic_Proc.Put_Line_Output (I'Img & " -> " & Romanic.Image(Nr)
                                    & " -> " & Na'Img);
        if Na /= I then
          Basic_Proc.Put_Line_Error ("Result does not match input");
          Basic_Proc.Set_Error_Exit_Code;
          return;
        end if;
      end;
    end loop;
    return;
  end if;

  for I in 1 .. Argument.Get_Nbre_Arg loop
    declare
      Str : constant String := Argument.Get_Parameter (Occurence => I);
      N : Romanic.Arabic;
    begin
      if Str(1) >= '0' and then Str(1) <= '9' then
        begin
          -- Try to make an arabic
          N := Romanic.Arabic'Value(Str);
        exception
          when others =>
            Basic_Proc.Put_Line_Error ("Not a valid arabic number: " & Str);
            Basic_Proc.Set_Error_Exit_Code;
            raise Skip_It;
        end;
        begin
          Basic_Proc.Put_Line_Output (Str & " -> "
              & Romanic.Image(Romanic.Arabic2Romanic(N)));
        exception
          when Error:others =>
            Basic_Proc.Put_Line_Error (
                "Arabic2Romanic on " & Str & " raised "
              & Ada.Exceptions.Exception_Name(Error));
            Basic_Proc.Set_Error_Exit_Code;
            raise Skip_It;
        end;
      else
        -- Shall be romanic
        begin
          N := Romanic.Romanic2Arabic(Romanic.Value(Str));
        exception
          when Romanic.Invalid_Romanic =>
            Basic_Proc.Put_Line_Error ("Not a valid romanic number: " & Str);
            Basic_Proc.Set_Error_Exit_Code;
            raise Skip_It;
          when Error:others =>
            Basic_Proc.Put_Line_Error (
                "Romanic2Arabic on " & Str & " raised "
              & Ada.Exceptions.Exception_Name(Error));
            Basic_Proc.Set_Error_Exit_Code;
            raise Skip_It;
        end;
        Basic_Proc.Put_Line_Output (Str & " -> "
                                  & Romanic.Arabic'Image(N));
      end if;
    exception
      when Skip_It =>
        null;
    end;

  end loop;

end T_Romanic;

