-- generic
with Ada.Text_Io;
function Int_Image16 (I : Int) return String is
  Str : String (1 .. Int'Width + 5);
  package Int_Io is new Ada.Text_Io.Integer_Io (Int);
begin
  Int_Io.Put (Str, I, Base => 16);
  for I in reverse Str'Range loop
    if Str(I) = ' ' then
      return Str(I + 1 .. Str'Last);
    end if;
  end loop;
  return Str;
exception
  when others =>
    raise Constraint_Error;
end Int_Image16;

