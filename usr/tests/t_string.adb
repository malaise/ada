with My_Io, String_Mng;
procedure T_String is

  Str : String(1 .. 500);
  Str_Len : Natural;
  Len : Positive;
  Align_Left : Boolean;
  Gap : Character;
  Trunc_Head : Boolean;
  Show_Trunc : Boolean;

begin

  loop
    My_Io.Put ("STR ? "); My_Io.Get_Line (Str, Str_Len);
    My_Io.Put ("LEN ? "); My_Io.Get(Len);
    My_Io.Put ("ALIGN_LEFT ? "); My_Io.Get(Align_Left);
    My_Io.Put ("GAP ? "); My_Io.Get(Gap);
    My_Io.Put ("TRUNC_HEAD ? "); My_Io.Get(Trunc_Head);
    My_Io.Put ("SHOW_TRUNC ? "); My_Io.Get(Show_Trunc);
    My_Io.Skip_Line;

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

