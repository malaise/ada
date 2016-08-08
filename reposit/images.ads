with Ada.Calendar;
with Long_Longs;
-- Various convertions from numbers to string
package Images is
  -- Image of an integer (without leading space)
  generic
    type Int is range <>;
  function Int_Image (I : Int) return String;

  -- Image of Integer
  function Integer_Image (I : Integer) return String;

  -- Image of Long_Integer and Long_Longs.Ll_Integer
  function Long_Image (L : Long_Integer) return String;
  function Llint_Image (L : Long_Longs.Ll_Integer) return String;

  -- Image of Integer in base 16 (without leading space)
  -- "16#XYZ#"
  generic
    type Int is range <>;
  function Int_Image16 (I : Int) return String;


  -- Image of a float (without leading space)
  generic
    type Real is digits <>;
  function Flo_Image (R : Real) return String;

  -- Image of Float
  function Float_Image (F : Float) return String;


  -- Image of an modular (without leading space)
  generic
    type Modul is mod <>;
  function Mod_Image (M : Modul) return String;
  -- Image of a Long_Longs.Llu_Natural
  function Llunat_Image (U : Long_Longs.Llu_Natural) return String;


  -- Image of a duration (without leading space)
  -- Specify the number of decimals (after the '.')
  -- Prepend a '+' or not to positive value
  function Dur_Image (Dur : Duration; Nb_Digits : Natural; Plus : Boolean)
                      return String;

  -- Return String image "Hh:Mm:Ss.mmm" (12 Characters) of a day duration
  function Dur_Image (Dur : Ada.Calendar.Day_Duration) return String;

  -- Return String image "YYyy/Mm/Dd Hh:Mm:Ss.mmm" of a time
  -- Alternatively uses  "YYyy-Mm-DdTHh:Mm:Ss.mmm", the ISO 8601 format
  -- 23 characters in both cases
  function Date_Image (Date : Ada.Calendar.Time;
                       Iso  : Boolean := False) return String;

end Images;

