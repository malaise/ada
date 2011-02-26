function Float_Image (R : Real) return String is
begin
  if R < 0.0 then
    return R'Img;
  else
    declare
      Str : constant String := R'Img;
    begin
      return Str(2 .. Str'Last);
    end;
  end if;
end Float_Image;

