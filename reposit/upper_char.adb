-- Convert a character into upper char
function Upper_Char (Char : Character) return Character is
  Offset  : constant Integer   := Character'Pos('A') - Character'Pos('a');
begin
  return (if Char not in 'a' .. 'z' then Char
          else Character'Val( Character'Pos(Char) + Offset));
end Upper_Char;

