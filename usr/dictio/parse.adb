with String_Mng;
function Parse (Str : String) return String is
  I : Natural;
begin
  I := String_Mng.Parse_Spaces (Str, False);
  return Str (1 .. I);
end Parse;

