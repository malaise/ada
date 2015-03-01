with Ada.Exceptions;
with As.U, Basic_Proc, Argument, Arbitrary, Num_Letters;
procedure T_Num_Letters is
  Num, Words : As.U.Asu_Us;
  N : Num_Letters.Number;
  Scale : Num_Letters.Scale_List;

  type Input_Kind_List is (Switch, Number, Word, Error);
  Kind : Input_Kind_List;

  function Image (N : Num_Letters.Number) return String is
    Str : constant String := Arbitrary.Image (N);
  begin
    return Str (2 ..Str'Last);
  end Image;

  procedure Put_Words is
  begin
    N := Num_Letters.Num_Of (Words.Image, Scale);
    Basic_Proc.Put_Line_Output (Image (N));
    -- Back to default scale
    Scale := Num_Letters.Long;
    Words.Set_Null;
--  exception
--    when Error:others =>
--      Scale := Num_Letters.Long;
--      Words.Set_Null;
--      Basic_Proc.Put_Line_Output (" Exception "
--        & Ada.Exceptions.Exception_Name (Error));
  end Put_Words;

begin
  -- Default scale
  Scale := Num_Letters.Long;
  for I in 1 .. Argument.Get_Nbre_Arg loop

    -- Get arg, set Kind
    declare
      Str : constant String := Argument.Get_Parameter (Occurence => I);
    begin
      if Str = "-l" then
        Scale := Num_Letters.Long;
        Kind := Switch;
      elsif Str = "-s" then
        -- Change scale for next num
        Scale := Num_Letters.Short;
        Kind := Switch;
      elsif Str(1) >= '0' and then Str(1) <= '9' then
        Num.Set (Str);
        Kind := Number;
      else
        if not Words.Is_Null then
        Words.Append (" ");
        end if;
        Words.Append (Str);
        Kind := Word;
      end if;
    end;

    if Kind /= Word and then not Words.Is_Null then
      -- End of words, put corresponding number
      Put_Words;
    end if;
    if Kind = Number then
      -- A number
      begin
        N := Arbitrary.Set (Num.Image);
      exception
        when Constraint_Error =>
          Basic_Proc.Put_Line_Output ("Invalid number");
          -- Back to default scale
          Scale := Num_Letters.Long;
          Kind := Error;
      end;
    end if;
    if Kind = Number then
      -- No error
      begin
        Basic_Proc.Put_Line_Output (Num_Letters.Letters_Of (N, Scale));
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

  if not Words.Is_Null then
    -- End of words, put corresponding number
    Put_Words;
  end if;

end T_Num_Letters;

