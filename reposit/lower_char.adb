-- Convert a character into lower char
function Lower_Char (Char : Character) return Character is
  Offset  : constant Integer   := Character'Pos('A') - Character'Pos('a');
begin

  if Char in 'A' .. 'Z' then
    return Character'Val( Character'Pos(Char) - Offset );
  else
    return Char;
  end if;

end Lower_Char;
