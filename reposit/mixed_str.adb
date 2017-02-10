-- Convert the characters of Str:
-- Any letter that follows a letter is lower char
-- Any other  letter (including the first letter) is UPPER char
with Upper_Char, Lower_Char;
function Mixed_Str (Str : String) return String is
  Result : String := Str;
  Prev_Separator : Boolean := True;
begin

  for C of Result loop
    if Prev_Separator and then C in 'a' .. 'z' then
      C := Upper_Char (C);
    elsif not Prev_Separator and then C in 'A' .. 'Z' then
      C := Lower_Char (C);
    end if;
    Prev_Separator := C not in 'a' .. 'z'
             and then C not in 'A' .. 'Z';
  end loop;
  return Result;

end Mixed_Str;

