-- Encode each Ascii char in two uppercase letters
with Argument;
with Types, Io_Manager;

procedure Letterator is

  -- 16 raws and 16 columns
  subtype Row_Range is Types.Letter range 'A' .. 'P';
  subtype Col_Range is Types.Letter range 'K' .. 'Z';

  type Letter_Pair_T is record
    Row : Row_Range;
    Col : Col_Range;
  end record;

  function Cpos (Char : Character) return Io_Manager.Byte is
  begin
    return Io_Manager.Byte(Character'Pos (Char));
  end Cpos;

  function Cval (B : Io_Manager.Byte) return Character is
  begin
    return Character'Val (Natural(B));
  end Cval;

  function Encode (B : Io_Manager.Byte) return Letter_Pair_T is
    use type Io_Manager.Byte;
  begin
    return (Row => Cval (Cpos (Row_Range'First) + (B / 16)),
            Col => Cval (Cpos (Col_Range'First) + (B rem 16)) );
  end Encode;

  function Decode (Pair : Letter_Pair_T) return Io_Manager.Byte is
    use type Io_Manager.Byte;
  begin
    return (Cpos (Pair.Row) - Cpos (Row_Range'First) ) * 16
         + (Cpos (Pair.Col) - Cpos (Col_Range'First) ) ;
  end Decode;

  procedure Usage is
  begin
    Io_Manager.Put_Line_Error ("Syntax error.");
    Io_Manager.Put_Line_Error (" Usage: "
      & Argument.Get_Program_Name
      & " -c | -d    [ -f<first_index> ] [ -l<last_index> ]");
    Io_Manager.Set_Error_Exit_Code;
  end Usage;

  Nb_Got_Arg : Natural := 1;
  Encode_Mode : Boolean;
  First_Offset, Last_Offset : Natural := 0;
  Byte, Prev_Byte : Io_Manager.Byte;
  Pair : Letter_Pair_T;
  Even : Boolean;

begin
  -- Parse arguments
  -- Mode must be defined
  begin
    if Argument.Get_Parameter (1, "c") /= "" then
      Usage;
      return;
    else
      Encode_Mode := True;
    end if;
  exception
    when Argument.Argument_Not_Found =>
      Encode_Mode := False;
  end;
  if not Encode_Mode then
    begin
      if Argument.Get_Parameter (1, "d") /= "" then
        Usage;
        return;
      end if;
    exception
      when Argument.Argument_Not_Found =>
        Usage;
        return;
    end;
  end if;
  Nb_Got_Arg := 1;

  -- Parse first and last offset
  begin
    First_Offset := Natural'Value (Argument.Get_Parameter (1, "f"));
    if First_Offset not in Positive then
      raise Constraint_Error;
    end if;
    Nb_Got_Arg := Nb_Got_Arg + 1;
  exception
    when Argument.Argument_Not_Found =>
      null;
    when Constraint_Error =>
      Usage;
      return;
  end;
  begin
    Last_Offset := Natural'Value (Argument.Get_Parameter (1, "l"));
    if Last_Offset not in Positive then
      raise Constraint_Error;
    end if;
    Nb_Got_Arg := Nb_Got_Arg + 1;
  exception
    when Argument.Argument_Not_Found =>
      null;
    when Constraint_Error =>
      Usage;
      return;
  end;
  if Nb_Got_Arg /= Argument.Get_Nbre_Arg then
    Usage;
    return;
  end if;

  -- Set first and last index
  if First_Offset /= 0 then
    -- Skip initial offset
    begin
      Io_Manager.Skip_To (First_Offset);
    exception
      when Io_Manager.End_Error =>
        return;
    end;
  end if;
  Io_Manager.Set_Skip_From (Last_Offset);

  -- Main loop
  Even := True;
  loop
    -- Read next byte until en of flow
    begin
      Byte := Io_Manager.Read;
    exception
      when Io_Manager.End_Error =>
        Io_Manager.Flush;
        exit;
    end;
    if Encode_Mode then
      Pair := Encode (Byte);
      Io_Manager.Write (Io_Manager.Byte(Cpos(Pair.Row)));
      Io_Manager.Write (Io_Manager.Byte(Cpos(Pair.Col)));
    else
      Even := not Even;
      if not Even then
        Prev_Byte := Byte;
      else
        Io_Manager.Write (Decode ( (Row => Cval(Prev_Byte),
                                    Col => Cval(Byte))));
      end if;
    end if;
  end loop;

  if not Encode_Mode and then not Even then
    Io_Manager.Put_Line_Error ("Unexpected end of input flow.");
    Io_Manager.Set_Error_Exit_Code;
    return;
  end if;

end Letterator;

