-- Convert the characters of Str into upper char
with Upper_Char;
function Upper_Str (Str : String) return String is
  Str_Loc : String (Str'Range);
begin

  for I in Str_Loc'Range loop
    Str_Loc(I) := Upper_Char (Str(I));
  end loop;
  return Str_Loc;

end Upper_Str;

