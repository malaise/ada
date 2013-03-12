-- Convert a character into lower char
function Lower_Char (Char : Character) return Character is
  Offset  : constant Integer   := Character'Pos('A') - Character'Pos('a');
begin
  return (if Char not in 'A' .. 'Z' then Char
          else Character'Val( Character'Pos(Char) - Offset));
end Lower_Char;

