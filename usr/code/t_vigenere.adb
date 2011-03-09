with Argument, Basic_Proc, Text_Line, Sys_Calls, As.U;
with Vigenere;
procedure T_Vigenere is

  Code : Boolean;
  Flow : Text_Line.File_Type;
  Str : As.U.Asu_Us;

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                        & "  -c | -d     <key>");
  end;

begin
  -- Check and get mode
  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;
  if Argument.Get_Parameter = "-c" then
    Code := True;
  elsif Argument.Get_Parameter = "-d" then
    Code := False;
  else
    Usage;
    return;
  end if;

  -- Open input flow
  Flow.Open (Text_Line.In_File, Sys_Calls.Stdin);

  -- Get input text
  loop
    declare
       Line : constant As.U.Asu_Us := Flow.Get;
    begin
      exit when Line.Is_Null;
      Str.Append (Line);
    end;
  end loop;
  Flow.Close;
  if Str.Is_Null then
    return;
  end if;

  declare
    Lstr : Vigenere.Long_String(1 .. Vigenere.Long_Positive (Str.Length));
  begin
    for I in 1 .. Str.Length loop
      Lstr(Vigenere.Long_Positive(I)) := Str.Element(I);
    end loop;
    if Code then
      Vigenere.Encode (Argument.Get_Parameter(Occurence => 2), Lstr);
    else
      Vigenere.Decode (Argument.Get_Parameter(Occurence => 2), Lstr);
    end if;
    for I in 1 .. Str.Length loop
      Str.Replace_Element(I, Lstr(Vigenere.Long_Positive(I)));
    end loop;
    Basic_Proc.Put_Output (Str.Image);
  end;

end T_Vigenere;

