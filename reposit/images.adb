with Ada.Text_Io;
with As.U, My_Math, Round_At, Str_Util, Normal, Day_Mng;
package body Images is

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

  -- Image of Long_Long_Integer
  function Loc_Long_Long_Image is new Int_Image (Long_Long_Integer);
  function Long_Long_Image (L : Long_Long_Integer) return String
           renames Loc_Long_Long_Image;


  -- Image of Integer in base 16 (without leading space)
  function Int_Image16 (I : Int) return String is
    Str : String (1 .. Int'Width + 5);
    package Int_Io is new Ada.Text_Io.Integer_Io (Int);
  begin
    Int_Io.Put (Str, I, Base => 16);
    for I in reverse Str'Range loop
      if Str(I) = ' ' then
        return Str(I + 1 .. Str'Last);
      end if;
    end loop;
    return Str;
  exception
    when others =>
      raise Constraint_Error;
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
    D := Duration (Round_At (My_Math.Real (Dur), -Nb_Digits));

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

  -- Return String image "YYyy/Mm/Dd Hh:Mm:Ss.mmm" of a time
  -- Alternatively uses  "YYyy-Mm-DdTHh:Mm:Ss.mmm", the ISO 8601 format
  -- 23 characters in both cases
  function Date_Image (Date : Ada.Calendar.Time;
                       Iso  : Boolean := False) return String is
    Year   : Ada.Calendar.Year_Number;
    Month  : Ada.Calendar.Month_Number;
    Day    : Ada.Calendar.Day_Number;
    Dur    : Ada.Calendar.Day_Duration;
    Hours  : Day_Mng.T_Hours;
    Mins   : Day_Mng.T_Minutes;
    Secs   : Day_Mng.T_Seconds;
    Millis : Day_Mng.T_Millisec;

    Sepd : Character := '/';
    Sepdt : Character := ' ';

  begin
    Ada.Calendar.Split (Date, Year, Month, Day, Dur);
    Day_Mng.Split (Dur, Hours, Mins, Secs, Millis);

    if Iso then
      Sepd := '-';
      Sepdt := 'T';
    end if;

    return Normal (Year,   4, Gap => '0') & Sepd
         & Normal (Month,  2, Gap => '0') & Sepd
         & Normal (Day,    2, Gap => '0') & Sepdt
         & Normal (Hours,  2, Gap => '0') & ':'
         & Normal (Mins,   2, Gap => '0') & ':'
         & Normal (Secs,   2, Gap => '0') & '.'
         & Normal (Millis, 3, Gap => '0');
  end Date_Image;

end Images;

