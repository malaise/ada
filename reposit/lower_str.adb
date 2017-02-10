-- Convert the characters of Str into lower char
with Lower_Char;
function Lower_Str (Str : String) return String is
  Result : String := Str;
begin

  for C of Result loop
    C := Lower_Char (C);
  end loop;
  return Result;

end Lower_Str;

