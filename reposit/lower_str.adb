-- Convert the characters of Str into lower char
with Lower_Char;
function Lower_Str (Str : String) return String is
  Str_Loc : String (Str'Range);
begin

  for I in Str_Loc'Range loop
    Str_Loc(I) := Lower_Char (Str(I));
  end loop;
  return Str_Loc;

end Lower_Str;

