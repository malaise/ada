with As.U, My_Math, Str_Util, Hexa_Utils, Normal, Day_Mng, Upper_Str, Mixed_Str;
package body Images is

  -- Image of a discrete type (Mixed_String)
  function Discr_Image (D : Discr) return String is
  begin
    return Mixed_Str (D'Img);
  end Discr_Image;

  -- Image of a Boolean (Mixed_String)
  function Loc_Boolean_Image is new Discr_Image (Boolean);
  function Boolean_Image (B : Boolean) return String renames Loc_Boolean_Image;

  -- Image of an integer (without leading space)
  function Int_Image (I : Int) return String is
  begin
    if I < 0 then
      return I'Img;
    else
      declare
        Str : constant String := I'Img;
      begin
        return Str(2 .. Str'Last);
      end;
    end if;
  end Int_Image;

  -- Image of Integer
  function Loc_Integer_Image is new Int_Image (Integer);
  function Integer_Image (I : Integer) return String renames Loc_Integer_Image;

  -- Image of Long_Integer
  function Loc_Long_Image is new Int_Image (Long_Integer);
  function Long_Image (L : Long_Integer) return String
           renames Loc_Long_Image;


  -- Image of Integer in base 16 (without leading space)
  -- 16#xxx#
  package body Int_Image16 is
    package Hexa_Image is new Hexa_Utils.Int_Image (Int);
    function Image (I : Int) return String is
    begin
      return "16#" & Upper_Str (Hexa_Image.Image (I)) & "#";
    end Image;
  end Int_Image16;

  -- Image of a float (without leading space)
  function Flo_Image (R : Real) return String is
  begin
    if R < 0.0 then
      return R'Img;
    else
      declare
        Str : constant String := R'Img;
      begin
        return Str(2 .. Str'Last);
      end;
    end if;
  end Flo_Image;

  -- Image of Float
  function Loc_Float_Image is new Flo_Image (Float);
  function Float_Image (F : Float) return String renames Loc_Float_Image;


  -- Image of an modular (without leading space)
  function Mod_Image (M : Modul) return String is
    Str : constant String := M'Img;
  begin
    return Str(2 .. Str'Last);
  end Mod_Image;


  -- Image of a duration (without leading space)
  function Dur_Image (Dur : Duration; Nb_Digits : Natural; Plus : Boolean)
                      return String is
     D : Duration;
     Str : As.U.Asu_Us;
     Dot : Natural;
  begin
    -- Round at Nb_Digits
    D := Duration (My_Math.Round_At (My_Math.Real (Dur),
                                     My_Math.Inte (-Nb_Digits)));

    -- Locate Dot
    Str := As.U.Tus (D'Img);
    Dot := Str_Util.Locate (Str.Image, ".");
    if Dot = 0 then
      -- No dot!?
      return Str.Image;
    end if;

    -- Remove tail
    if Nb_Digits /= 0 then
      -- 0 digits => Remove dot
      -- Else remove from Dot + NbDigits + 1
      Dot := Dot + Nb_Digits + 1;
      if Dot > Str.Length then
        -- Less digits than required
        Dot := 0;
      end if;
    end if;
    if Dot /= 0 then
      Str.Delete (Dot, Str.Length);
    end if;

    -- Strip leading space or put a '+'
    if Str.Element (1) = ' ' then
      if Plus then
        Str.Replace_Element (1, '+');
      else
        Str.Delete (1, 1);
      end if;
    end if;
    return Str.Image;
  end Dur_Image;

  -- Return String image "Hh:Mm:Ss.mmm" (12 Characters) of a day duration
  -- On option, separate milliseconds by a comma instead of dot
  --  (ISO 8601 recommends to use comma and tolerates dot)
  function Dur_Image (Dur   : Ada.Calendar.Day_Duration;
                      Comma : Boolean := False) return String is
    Hours  : Day_Mng.T_Hours;
    Mins   : Day_Mng.T_Minutes;
    Secs   : Day_Mng.T_Seconds;
    Millis : Day_Mng.T_Millisecs;
  begin
    Day_Mng.Split (Dur, Hours, Mins, Secs, Millis);
    return Normal (Hours,  2, Gap => '0') & ':'
         & Normal (Mins,   2, Gap => '0') & ':'
         & Normal (Secs,   2, Gap => '0') & (if Comma then ',' else '.')
         & Normal (Millis, 3, Gap => '0');
  end Dur_Image;

  -- Return String image "YYyy-Mm-DdTHh:Mm:Ss.mmm", ISO 8601 format with dot
  -- On option, uses     "YYyy-Mm-DdTHh:Mm:Ss,mmm", ISO 8601 format with comma
  --  (ISO 8601 recommends to use comma and tolerates dot)
  -- 23 characters in all cases
  function Date_Image (Date : Ada.Calendar.Time;
                       Comma : Boolean := False) return String is
    Year   : Ada.Calendar.Year_Number;
    Month  : Ada.Calendar.Month_Number;
    Day    : Ada.Calendar.Day_Number;
    Dur    : Ada.Calendar.Day_Duration;

  begin
    Ada.Calendar.Split (Date, Year, Month, Day, Dur);

    -- Iso separators within date and between date and time
    return Normal (Year,   4, Gap => '0') & '-'
         & Normal (Month,  2, Gap => '0') & '-'
         & Normal (Day,    2, Gap => '0') & 'T'
         & Dur_Image (Dur, Comma);
  end Date_Image;

end Images;

