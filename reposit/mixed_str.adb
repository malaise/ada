with Upper_Char, Lower_Char;
function Mixed_Str (Str : String) return String is
  Str_Loc : String (Str'Range) := Str;
  Prev_Separator : Boolean := True;
begin

  for I in Str_Loc'Range loop
    if Prev_Separator and then Str_Loc(I) in 'a' .. 'z' then
      Str_Loc(I) := Upper_Char (Str_Loc(I));
    elsif not Prev_Separator and then Str_Loc(I) in 'A' .. 'Z' then
      Str_Loc(I) := Lower_Char (Str_Loc(I));
    end if;
    Prev_Separator := Str_Loc(I) not in 'a' .. 'z'
             and then Str_Loc(I) not in 'A' .. 'Z';
  end loop;

  return Str_Loc;

end Mixed_Str;

