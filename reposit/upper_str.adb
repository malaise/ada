-- Convert the characters of Str into upper char
with Upper_Char;
function Upper_Str (Str : String) return String is
  Result : String := Str;
begin

  for C of Result loop
    C := Upper_Char (C);
  end loop;
  return Result;

end Upper_Str;

