with Images;
package body Long_Longs is

    -- Image of Ll_Int
  function Llint_Image is new Images.Int_Image (Ll_Integer);
  function Image (L : Ll_Integer) return String renames Llint_Image;

  -- Image of Llu_Natural
  function Lunat_Image is new Images.Mod_Image (Llu_Natural);
  function Image (U : Long_Longs.Llu_Natural) return String
           renames Lunat_Image;

end Long_Longs;
