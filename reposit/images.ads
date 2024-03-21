-- Various convertions from numbers to string
with Ada.Calendar;
package Images is

  -- Image of a discrete type (Mixed_String)
  generic
    type Discr is (<>);
  function Discr_Image (D : Discr) return String;

  -- Image of a Boolean (Mixed_String)
  function Boolean_Image (B : Boolean) return String;

  -- Image of an Integer (without leading space)
  generic
    type Int is range <>;
  function Int_Image (I : Int) return String;

  -- Image of an Integer
  function Integer_Image (I : Integer) return String;

  -- Image of a Long_Integer
  function Long_Image (L : Long_Integer) return String;

  -- Image of an integer in base 16 (without leading space)
  -- "16#XYZ#"
  generic
    type Int is range <>;
  package Int_Image16 is
    function Image (I : Int) return String;
  end Int_Image16;

  -- Image of a float (without leading space)
  generic
    type Real is digits <>;
  function Flo_Image (R : Real) return String;

  -- Image of a Float
  function Float_Image (F : Float) return String;


  -- Image of a modular (without leading space)
  generic
    type Modul is mod <>;
  function Mod_Image (M : Modul) return String;


  -- Image of a Duration (without leading space)
  -- Specify the number of decimals (after the '.')
  -- Prepends a '+' or not to positive value
  function Dur_Image (Dur : Duration; Nb_Digits : Natural; Plus : Boolean)
                      return String;

  -- Image of a Day_Duration (without leading space)
  -- Returns String image "Hh:Mm:Ss.mmm" (12 Characters) of a day duration
  -- On option, separates milliseconds by a comma instead of dot
  --  (ISO 8601 recommends to use comma and tolerates dot)
  function Dur_Image (Dur   : Ada.Calendar.Day_Duration;
                      Comma : Boolean := False) return String;

  -- Image of a Time
  -- Returns String image "YYyy-Mm-DdTHh:Mm:Ss.mmm", ISO 8601 format with dot
  -- On option, uses      "YYyy-Mm-DdTHh:Mm:Ss,mmm", ISO 8601 format with comma
  --  (ISO 8601 recommends to use comma and tolerates dot)
  -- 23 characters in all cases
  function Date_Image (Date : Ada.Calendar.Time;
                       Comma : Boolean := False) return String;

end Images;

