-- Test romanic 2 arabic conversion
-- With no arg, loops on all values (1 to 3999)
--  otherwise converts arguments
with Ada.Exceptions;
with Basic_Proc, Argument, Romanic;

procedure T_Romanic is
  N : Romanic.Arabic_Range;
  Skip_It : exception;
begin

  if Argument.Get_Nbre_Arg = 0 then
    for I in Romanic.Arabic_Range loop
      declare
        Str : constant String := Romanic.Arabic2Romanic(I);
        N : constant Romanic.Arabic_Range := Romanic.Romanic2Arabic(Str);
      begin
        Basic_Proc.Put_Line_Output (I'Img & " -> " & Str & " -> " & N'Img);
      end;
    end loop;
    return;
  end if;

  for I in 1 .. Argument.Get_Nbre_Arg loop
    declare
      Str : constant String := Argument.Get_Parameter (Occurence => I);
    begin
      if Str(1) >= '0' and then Str(1) <= '9' then
        begin
          -- Try to make an arabic
          N := Romanic.Arabic_Range'Value(Str);
        exception
          when others =>
            Basic_Proc.Put_Line_Output ("Not a valid arabic number: " & Str);
            raise Skip_It;
        end;
        begin
          Basic_Proc.Put_Line_Output (Str & " -> " & Romanic.Arabic2Romanic(N));
        exception
          when Error:others =>
            Basic_Proc.Put_Line_Output (
                "Arabic2Romanic on " & Str & " raised "
              & Ada.Exceptions.Exception_Name(Error));
            raise Skip_It;
        end;
      else
        -- Shall be romanic
        begin
          N := Romanic.Romanic2Arabic(Str);
        exception
          when Romanic.Invalid_Romanic =>
            Basic_Proc.Put_Line_Output ("Not a valid romanic number: " & Str);
            raise Skip_It;
          when Error:others =>
            Basic_Proc.Put_Line_Output (
                "Romanic2Arabic on " & Str & " raised "
              & Ada.Exceptions.Exception_Name(Error));
            raise Skip_It;
        end;
        Basic_Proc.Put_Line_Output (Str & " -> "
                                  & Romanic.Arabic_Range'Image(N));
      end if;
    exception
      when Skip_It =>
        null;
    end;

  end loop;

end T_Romanic;

