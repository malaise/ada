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

begin

  loop
    My_Io.Put ("Str ? "); My_Io.Get_Line (Str, Str_Len);
    My_Io.Put ("Len ? "); My_Io.Get(Len);
    My_Io.Put ("Align_Left ? "); My_Io.Get(Align_Left);
    My_Io.Put ("Gap ? "); My_Io.Get(Gap);
    My_Io.Put ("Trunc_Head ? "); My_Io.Get(Trunc_Head);
    My_Io.Put ("Show_Trunc ? "); My_Io.Get(Show_Trunc);
    My_Io.Put ("Case (ULMA) ? "); My_Io.Get(Case_Char);
    My_Io.Skip_Line;

    Case_Char := Upper_Char (Case_Char);
    if Case_Char = 'U' then
      Str(1 .. Str_Len) := Upper_Str (Str(1 .. Str_Len));
    elsif Case_Char = 'L' then
      Str(1 .. Str_Len) := Lower_Str (Str(1 .. Str_Len));
    elsif Case_Char = 'M' then
      Str(1 .. Str_Len) := Mixed_Str (Str(1 .. Str_Len));
    end if;

    My_Io.Put_Line ("String: |" & Str(1 .. Str_Len) & "|   len: "
                  & Integer'Image(Str_Len));
    My_Io.Put_Line (
        "First: " 
      & Natural'Image(String_Mng.Parse_Spaces(Str(1 .. Str_Len), True))
      & " Last: "
      & Natural'Image(String_Mng.Parse_Spaces(Str(1 .. Str_Len), False)));

    My_Io.Put_Line (
        "Procuste: |" 
      & String_Mng.Procuste(Str(1 .. Str_Len), Len, Align_Left, Gap,
                            Trunc_Head, Show_Trunc)
      & "|");
    My_Io.New_Line;
  end loop;
end T_String;

