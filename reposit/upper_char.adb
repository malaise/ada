function Upper_Char (Char : Character) return Character is
  Offset  : constant Integer   := Character'Pos('A') - Character'Pos('a');
begin

  if Char in 'a' .. 'z' then
    return Character'Val( Character'Pos(Char) + Offset );
  else
    return Char;
  end if;
end Upper_Char;
