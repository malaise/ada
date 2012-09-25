with Str_Util;
function Parse (Str : String) return String is
  I : Natural;
begin
  I := Str_Util.Parse_Spaces (Str, False);
  return Str (Str'First .. I);
end Parse;

