function Upper_Str (Str : String) return String is
  Offset  : constant Integer   := Character'Pos('A') - Character'Pos('a');
  Str_Loc : String (Str'Range) := Str;
begin

  for I in Str_Loc'Range loop
    if Str_Loc(I) in 'a' .. 'z' then
      Str_Loc(I) := Character'Val( Character'Pos(Str_Loc(I)) + Offset );
    end if;
  end loop;

  return Str_Loc;

end Upper_Str;

