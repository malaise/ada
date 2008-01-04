-- Image of an modular (without leading space)
function Mod_Image (M : Modul) return String is
  Str : constant String := M'Img;
begin
  return Str(2 .. Str'Last);
end Mod_Image;

