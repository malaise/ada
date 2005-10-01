with Ada.Text_Io;
with Argument;
with Vigenere;

procedure T_Vigenere is

  Code : Boolean;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
                        & "  -c | -d     <key> <text>");
  end;

begin
  if Argument.Get_Nbre_Arg /= 3 then
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

  declare
    Str : String := Argument.Get_Parameter (Occurence => 3);
    Lstr : Vigenere.Long_String (1 .. Str'Length);
  begin
    for I in Str'Range loop
      Lstr(Vigenere.Long_Positive(I)) := Str(I);
    end loop;
    if Code then
      Vigenere.Encode (Argument.Get_Parameter(Occurence => 2), Lstr);
    else
      Vigenere.Decode (Argument.Get_Parameter(Occurence => 2), Lstr);
    end if;
    for I in Str'Range loop
      Str(I) := Lstr(Vigenere.Long_Positive(I));
    end loop;
    Ada.Text_Io.Put_Line (Str);
  end;

end T_Vigenere;

