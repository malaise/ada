function Int_Image (I : Int) return String is
begin
  if I < 0 then
    return I'Img;
  else
    declare
      Str : constant String := I'Img;
    begin
      return Str(2 .. Str'Last);
    end;
  end if;
end Int_Image;

