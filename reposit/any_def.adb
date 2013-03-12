with Images, Mixed_Str, Address_Ops;
package body Any_Def is

  function Integer_Image is new Images.Int_Image (Integer);
  function Inte_Image is new Images.Int_Image (My_Math.Inte);
  function Flo_Image is new Images.Flo_Image (Float);
  function Real_Image is new Images.Flo_Image (My_Math.Real);


  function Image (A : Any) return String is
  begin
    return (case A.Kind is
        when None_Kind    => "",
        when Bool_Kind    => Mixed_Str (A.Bool'Img),
        when Integer_Kind => Integer_Image (A.Int),
        when Inte_Kind    => Inte_Image (A.Inte),
        when Real_Kind    => Real_Image (A.Real),
        when Float_Kind   => Flo_Image (A.Flo),
        when Str_Kind     => A.Str.Image,
        when Address_Kind => Address_Ops.Image (A.Addr),
        when Access_Kind  => "->" & Image(A.Acc.all) );
  end Image;

end Any_Def;

