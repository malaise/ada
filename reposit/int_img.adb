-- Shortcut to string image of an integer
with Images;
function Int_Img (I : Integer) return String is
pragma Inline (Int_Img);
begin
  return Images.Integer_Image (I);
end Int_Img;

