with Images, Mixed_Str, Address_Ops;
package body Any_Def is

  function Integer_Image is new Images.Int_Image (Integer);
  function Inte_Image is new Images.Int_Image (My_Math.Inte);
  function Flo_Image is new Images.Flo_Image (Float);
  function Real_Image is new Images.Flo_Image (My_Math.Real);


  function Image (A : Any) return String is
  begin
    case A.Kind is
      when None_Kind    => return "";
      when Bool_Kind    => return Mixed_Str (A.Bool'Img);
      when Integer_Kind => return Integer_Image (A.Int);
      when Inte_Kind    => return Inte_Image (A.Inte);
      when Real_Kind    => return Real_Image (A.Real);
      when Float_Kind   => return Flo_Image (A.Flo);
      when Str_Kind     => return A.Str.Image;
      when Address_Kind => return Address_Ops.Image (A.Address);
      when Access_Kind  => return "->" & Image(A.Acc.all);
    end case;
  end Image;

end Any_Def;

