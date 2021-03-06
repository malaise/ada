with Utf_8, Utf_16, Argument, Basic_Proc, Gets, Hexa_Utils, Upper_Str;
procedure T_Utf is

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
       & " <Unicode> | <Utf>");
    Basic_Proc.Put_Line_Error (" <Unicode> ::= U+<hexanum>");
    Basic_Proc.Put_Line_Error (
       " <Utf>     ::= -utf8 | -utf16    { <hexanum> }");
  end Usage;

  Abort_Error : exception;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR : " & Msg);
    Usage;
    Basic_Proc.Set_Error_Exit_Code;
    raise Abort_Error;
  end Error;

  function Get_Value (Str : String) return Natural is
    (Gets.Get_Int ("16#" & Str & "#"));

  procedure Put (Str : in String) renames Basic_Proc.Put_Output;
  procedure Put (N : in Natural; Width : in Positive) is
  begin
    Put (Upper_Str (Hexa_Utils.Image (N, Width)));
  end Put;

  Nb_Arg : Natural;

  type Mode_List is (Unicode, Utf8, Utf16);
  Mode : Mode_List;
  Unicode_Val : Utf_8.Unicode_Number;
  Codes : array (1 .. Utf_8.Max_Chars) of Natural;
  Nb_Codes : Natural;
begin

  -- Parse arguments
  Nb_Arg := Argument.Get_Nbre_Arg;
  begin
    if Nb_Arg = 0 then
      Usage;
      return;
    elsif Nb_Arg = 1 then
      -- A Unicode
      declare
        Str : constant String := Argument.Get_Parameter (1);
      begin
        if Str(1 .. 2) = "U+" then
          Mode := Unicode;
          Unicode_Val := Get_Value (Str (3 .. Str'Last));
        else
          raise Constraint_Error;
        end if;
      end;
    elsif Argument.Get_Parameter (1) = "-utf8" then
      -- Up to 4 Utf8 codes
      Mode := Utf8;
      if Nb_Arg < 2 or else Nb_Arg  - 1 > Utf_8.Max_Chars then
        raise Constraint_Error;
      end if;
      Nb_Codes := Nb_Arg - 1;
      for I in 2 .. Nb_Arg loop
       Codes(I-1) := Get_Value (Argument.Get_Parameter(I));
       if Codes(I-1) > 16#FF# then
         raise Constraint_Error;
       end if;
     end loop;
    elsif Argument.Get_Parameter (1) = "-utf16" then
      -- One or 2 Utf16 codes
      Mode := Utf16;
      if Nb_Arg < 2 or else Nb_Arg  - 1 > Utf_16.Max_Chars then
        raise Constraint_Error;
      end if;
      Nb_Codes := Nb_Arg - 1;
      for I in 2 .. Nb_Arg loop
       Codes(I-1) := Get_Value (Argument.Get_Parameter(I));
       if Codes(I-1) > 16#FFFF# then
         raise Constraint_Error;
       end if;
     end loop;
    else
      raise Constraint_Error;
    end if;
  exception
    when others =>
      Error ("Invalid argument");
  end;

  -- Convert Utf to Unicode
  if Mode = Utf8 then
    declare
      Word : Utf_8.Word(1 .. Nb_Codes);
    begin
      for I in 1 ..  Nb_Codes loop
        Word(I) := Character'Val (Codes(I));
      end loop;
      Unicode_Val := Utf_8.Decode (Word);
    exception
      when Utf_8.Invalid_Sequence =>
        Basic_Proc.Put_Line_Output ("Invalid sequence");
        return;
    end;
  elsif Mode = Utf16 then
    declare
      Word : Utf_16.Word(1 .. Nb_Codes);
    begin
      for I in 1 .. Nb_Codes loop
        Word(I) := Wide_Character'Val (Codes(I));
      end loop;
      Unicode_Val := Utf_16.Decode (Word);
    exception
      when Utf_16.Invalid_Sequence =>
        Basic_Proc.Put_Line_Output ("Invalid sequence");
        return;
    end;
  end if;

  -- Put unicode and conversion in Utf-16 and Utf-8
  --  and corresponding Char (Utf-8)
  Put ("U+");
  Put (Unicode_Val, 6);
  Put (" : ");
  begin
    declare
      Str16 : constant Utf_16.Sequence := Utf_16.Encode (Unicode_Val);
    begin
      Put (Wide_Character'Pos (Str16(1)), 4);
      Put (" ");
      if Str16'Length = 2 then
        Put (Wide_Character'Pos (Str16(2)), 4);
      else
        Put ("    ");
      end if;
    end;
  exception
    when Utf_16.Excluded_Non_Character =>
      Put ("Excluded ");
  end;
  Put (" : ");

  declare
    Str8 : constant Utf_8.Sequence := Utf_8.Encode (Unicode_Val);
  begin
    for I in 1 .. Utf_8.Max_Chars loop
      if I <= Str8'Last then
        Put (Character'Pos (Str8(I)), 2);
      else
        Put ("  ");
      end if;
      Put (" ");
    end loop;
    Put (": " & Str8);
  end;
  Basic_Proc.New_Line_Output;

exception
  when Abort_Error =>
    null;
end T_Utf;

