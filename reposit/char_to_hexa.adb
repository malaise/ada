-- Converts an hexadecimal character (0..9 | 'a' .. 'f' | 'A' .. 'F')
--  into its value (0 .. 15).
-- Raises Constraint_Error if invalid character.
function Char_To_Hexa (C : Character) return Natural is
begin
  case C is
    when '0' .. '9' =>
      return Character'Pos(C) - Character'Pos('0');
    when 'a' .. 'f' =>
      return Character'Pos(C) - Character'Pos('a') + 16#0A#;
    when 'A' .. 'F' =>
      return Character'Pos(C) - Character'Pos('A') + 16#0A#;
    when others =>
      raise Constraint_Error;
  end case;
end Char_To_Hexa;

