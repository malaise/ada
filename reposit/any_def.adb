with Images, Mixed_Str, Address_Ops;
package body Any_Def is

  function Integer_Image (I : Integer) return String
                         renames Images.Integer_Image;
  function Lint_Image (L : Long_Longs.Ll_Integer) return String
                      renames Images.Ll_Integer_Image;
  function Flo_Image (F : Float) return String renames Images.Float_Image;
  function Real_Image is new Images.Flo_Image (My_Math.Real);


  function Image (A : Any) return String is
  begin
    return (case A.Kind is
        when None_Kind     => "",
        when Bool_Kind     => Mixed_Str (A.Bool'Img),
        when Trilean_Kind  => Trilean.Image (A.Tril),
        when Integer_Kind  => Integer_Image (A.Int),
        when Lint_Kind     => Lint_Image (A.Lint),
        when Duration_Kind => Images.Dur_Image (A.Dur, 9, False),
        when Float_Kind    => Flo_Image (A.Flo),
        when Real_Kind     => Real_Image (A.Real),
        when Str_Kind      => A.Str.Image,
        when Address_Kind  => Address_Ops.Image (A.Addr),
        when Access_Kind   => "->" & Image(A.Acc.all) );
  end Image;

end Any_Def;

