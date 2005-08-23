with My_Io, String_Mng, Upper_Str, Lower_Str, Mixed_Str, Upper_Char;
procedure T_String is

  Str : String(1 .. 500);
  Str_Len : Natural;
  Len : Positive;
  Align_Left : Boolean;
  Gap : Character;
  Trunc_Head : Boolean;
  Show_Trunc : Boolean;
  Case_Char : Character;

  procedure Bool_Get (V : out Boolean) is
    Str : String (1 .. 80);
    Len : Integer;
  begin
    loop
      My_Io.Get_Line (Str, Len);
      Str(1 .. Len) := Mixed_Str (Str(1 .. Len));
      if Str(1 .. Len) = "True" or else Str(1 .. Len) = "T"
      or else Str(1 .. Len) = "Yes" or else Str(1 .. Len) = "Y" then
        V := True;
        return;
      elsif Str(1 .. Len) = "False" or else Str(1 .. Len) = "F"
      or else Str(1 .. Len) = "No" or else Str(1 .. Len) = "N" then
        V := False;
        return;
      end if;
      My_Io.Put ("Yes or No ? ");
    end loop;
  end Bool_Get;

     
  

begin

  loop
    My_Io.Put ("Str (String)? "); My_Io.Get_Line (Str, Str_Len);
    My_Io.Put_Line ("String: |" & Str(1 .. Str_Len) & "|   len: "
                  & Integer'Image(Str_Len));
    My_Io.Put_Line (
        "First: " 
      & Natural'Image(String_Mng.Parse_Spaces(Str(1 .. Str_Len), True))
      & " Last: "
      & Natural'Image(String_Mng.Parse_Spaces(Str(1 .. Str_Len), False)));

    My_Io.Put ("Case conversion (ULMA) ? "); My_Io.Get(Case_Char);
    My_Io.Skip_Line;
    Case_Char := Upper_Char (Case_Char);
    if Case_Char = 'U' then
      My_Io.Put_line (Upper_Str (Str(1 .. Str_Len)));
    elsif Case_Char = 'L' then
      My_Io.Put_line (Lower_Str (Str(1 .. Str_Len)));
    elsif Case_Char = 'M' then
      My_Io.Put_line (Mixed_Str (Str(1 .. Str_Len)));
    end if;

    My_Io.Put ("Procusre Len (Int)? "); My_Io.Get(Len); My_Io.Skip_Line;
    My_Io.Put ("Align_Left (YN)? "); Bool_Get(Align_Left);
    My_Io.Put ("Gap (Char)? "); My_Io.Get(Gap); My_Io.Skip_Line;
    My_Io.Put ("Trunc_Head (YN)? "); Bool_Get(Trunc_Head);
    My_Io.Put ("Show_Trunc (YN)? "); Bool_Get(Show_Trunc);

    My_Io.Put_Line (
        "Procuste: |" 
      & String_Mng.Procuste(Str(1 .. Str_Len), Len, Align_Left, Gap,
                            Trunc_Head, Show_Trunc)
      & "|");
    My_Io.New_Line;
  end loop;
end T_String;

