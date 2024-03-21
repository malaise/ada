with Images, Mixed_Str, Address_Ops;
package body Any_Def is

  function Real_Image is new Images.Flo_Image (My_Math.Real);

  function Image (A : Any) return String is
    (case A.Kind is
       when None_Kind      => "",
       when Bool_Kind      => Mixed_Str (A.Bool'Img),
       when Trilean_Kind   => Trilean.Image (A.Tril),
       when Integer_Kind   => Images.Integer_Image (A.Int),
       when Lint_Kind      => Long_Longs.Image (A.Lint),
       when Arbitrary_Kind => Arbitrary.Basic_Image (A.Arbi),
       when Duration_Kind  => Images.Dur_Image (A.Dur, 9, False),
       when Float_Kind     => Images.Float_Image (A.Flo),
       when Real_Kind      => Real_Image (A.Real),
       when Str_Kind       => A.Str.Image,
       when Address_Kind   => Address_Ops.Image (A.Addr),
       when Access_Kind    => "->" & Image(A.Acc.all) );

end Any_Def;

