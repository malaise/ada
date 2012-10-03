with Ada.Calendar;
package Images is
  -- Image of an integer (without leading space)
  generic
    type Int is range <>;
  function Int_Image (I : Int) return String;

  -- Image of Integer
  function Integer_Image (I : Integer) return String;

  -- Image of Long_Long_Integer
  function Long_Image (L : Long_Long_Integer) return String;

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


  -- Image of a duration (without leading space)
  function Dur_Image (Dur : Duration; Nb_Digits : Natural; Plus : Boolean)
                      return String;

  -- Return String image "YYyy/Mm/Dd Hh:Mm:Ss.mmm" of a time
  -- Alternatively uses  "YYyy-Mm-DdTHh:Mm:Ss.mmm", the ISO 8601 format
  -- 23 characters in both cases
  function Date_Image (Date : Ada.Calendar.Time;
                       Iso  : Boolean := False) return String;

end Images;

