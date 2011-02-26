with Int_Image, Float_Image, Mixed_Str;
package body Any_Def is

  function Inte_Image is new Int_Image (My_Math.Inte);
  function Real_Image is new Float_Image (My_Math.Real);


  function Image (A : Any) return String is
  begin
    case A.Kind is
      when None_Kind => return "";
      when Bool_Kind => return Mixed_Str (A.Bool'Img);
      when Inte_Kind => return Inte_Image (A.Inte);
      when Real_Kind => return Real_Image (A.Real);
      when Str_Kind  => return A.Str.Image;
    end case;
  end Image;

end Any_Def;

