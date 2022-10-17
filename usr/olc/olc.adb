with Ada.Text_Io, Ada.Characters.Handling, Ada.Strings.Fixed,
     Ada.Environment_Variables, Ada.Numerics.Generic_Elementary_Functions;
package body Olc is

  package Real_Math is new Ada.Numerics.Generic_Elementary_Functions (Real);
  subtype Inte is Long_Long_Integer;

  -- Log traces
  package Logger is
    procedure Init (Unused_Name : String);
    procedure Log_Debug (Msg : in String);
  end Logger;
  package body Logger is
    Inited :  Boolean := False;
    Debug :  Boolean := False;
    procedure Init (Unused_Name : String) is
      Trace_Name : constant String := "OLC_TRACE";
    begin
      if Inited then
        -- Init only once
        return;
      end if;
      Debug := Ada.Environment_Variables.Exists (Trace_Name)
               and then Ada.Environment_Variables.Value (Trace_Name) = "Debug";
      Inited := True;
    end Init;
    procedure Log_Debug (Msg : in String) is
    begin
      if Debug then
        Ada.Text_Io.Put_Line ("OLC ==> " & Msg);
      end if;
    end Log_Debug;
  end Logger;

  -- Convertion to upper char and string
  function Upper_Char (Item : in Character) return Character
           renames Ada.Characters.Handling.To_Upper;
  function Upper_Str (Item : in String) return String
           renames Ada.Characters.Handling.To_Upper;

  -- Locate a char in a string
  function Locate (Within     : String;
                   Char       : Character;
                   From_Index : Positive := 1) return Natural is
    (Ada.Strings.Fixed.Index (Within, Char & "", From_Index));

  -- Trunk Real into Inte
  function Trunc (X : Real) return Inte is
    Epsilon : constant Real := Real'Model_Epsilon;
    Int : Inte;
    Rea : Real;
    use Real_Math;
  begin
    if      X > Real (Inte'Last)
    or else X < Real (Inte'First) then
      raise Constraint_Error;
    end if;

    -- Specific case
    if X = 0.0 then
      return 0;
    end if;

    -- Round
    Int := Inte (X);
    Rea := Real (Int);

    -- If round leads to a delta of less than Eps, then it is correct
    if abs (Rea - X) / 10.0 ** Log (abs X, 10.0) < Epsilon then
      return Int;
    end if;

    -- Adjust +/- 1 due to conversion to Inte
    if X > 0.0 then
      -- if x > 0 error by exceed
      if Rea > X then
        Int := Int - 1;
      end if;
    elsif X < 0.0 then
      -- if x < 0 error by default
      if Rea < X then
        Int := Int + 1;
      end if;
    else
      Int := 0;
    end if;

    return Int;
  end Trunc;


  -- Max lat and long
  Min_Lat : constant Lat_Range := Lat_Range'First;
  Max_Lat : constant Lat_Range := Lat_Range'Last;
  Min_Lon : constant Lon_Range := Lon_Range'First;
  Max_Lon : constant Lon_Range := Lon_Range'Last;

  -- Encoding base
  Base : constant Inte := 20;

  -- Precisions for pairs and grid
  Last_Precision : constant Inte := Inte (Precision_Range'Last);
  Pair_Precision : constant Inte := 10;
  Pair_Precision_Inverse : constant Inte := 8000;
  Grid_Precision : constant Inte := Last_Precision - Pair_Precision;
  Grid_Cols : constant Inte := 4;
  Grid_Rows  : constant Inte := Base / Grid_Cols;
  Grid_Lat_Precision_Inverse : constant Inte
      := Pair_Precision_Inverse * Grid_Rows ** Integer (Grid_Precision);
  Grid_Lon_Precision_Inverse : constant Inte
      := Pair_Precision_Inverse * Grid_Cols ** Integer (Grid_Precision);

  -- Some useful math operations
  function "**" (N, E : Inte) return Inte is (N ** Natural (E));

  -- Padding and separator
  Pad : constant Character := '0';
  Sep : constant Character := '+';
  Sep_Pos : constant Natural := Code_Length'First;

  -- LOcal: Init logger
  procedure Init_Logger is
  begin
    Logger.Init ("Olc");
  end Init_Logger;

  -- Character lookup table for 'C' to X
  -- Codes contain the characters 2 3 4 5 6 7 8 9 C F G H J M P Q R V W X
  Alphabet : constant String (1 .. Natural (Base)) := "23456789CFGHJMPQRVWX";
  -- Not case sensitive
  subtype Letter_Range is Character range 'C' .. 'X';
  No_Index : constant Integer := -1;
  Last_Index : constant Integer := 19;
  subtype Index_Range is Integer range No_Index .. Last_Index;
  Lookup : constant array (Letter_Range) of Index_Range := (
    'C' =>  8, 'F' =>  9, 'G' => 10, 'H' => 11, 'J' => 12, 'M' => 13,
    'P' => 14, 'Q' => 15, 'R' => 16, 'V' => 17, 'W' => 18, 'X' => 19,
    others => No_Index);

  -- Local: Get alphabet index
  function Get_Index (Char : Character) return Index_Range is
    Res : Index_Range;
  begin
    case Char is
      when 'C' .. 'X' => Res := Lookup (Char);
      when 'c' .. 'x' => Res := Lookup (Upper_Char (Char));
      when '2' .. '9' => Res := Character'Pos (Char) - Character'Pos ('2');
      when others     => Res := No_Index;
    end case;
    return Res;
  end Get_Index;

  -- Local: Precision for a given "length" in digits
  --  Lengths <= 10 have the same precision for latitude and longitude,
  --  but lengths > 10 have different precisions due to the grid method having
  --  fewer columns than rows.
  function Precision_Of (Precision : Precision_Range) return Real is
  begin
    if Precision <= 10 then
      return Real (Base) ** (Precision / (-2) + 2);
    else
      return Real (Base) ** (-3) / 5.0 ** (Precision - 10);
    end if;
  end Precision_Of;

  -- Local: Normalize a longitude into the range -180 to 180, not including 180.
  function Normalize_Lon (Longitude : Real) return Lon_Range is
    Lon : Real := Longitude;
  begin
    while Lon < Min_Lon loop
      Lon := Lon + 360.0;
    end loop;
    while Lon >= Max_Lon loop
      Lon := Lon - 360.0;
    end loop;
    return Lon;
  end Normalize_Lon;

  -- Local: Adjusts 90 degree latitude to be lower so that a legal OLC code can
  --  be generated.
  function Adjust_Lat (Latitude : Lat_Range;
                       Precision : Precision_Range) return Lat_Range is
    Lat : Real := Latitude;
  begin
    if Lat < Min_Lat then
      Lat := Min_Lat;
    end if;
    if Lat < Max_Lat then
      return Lat;
    end if;
    -- Lat = 90
    return Lat - Precision_Of (Precision) / 2.0;
  end Adjust_Lat;

  -- Local: Clean code from pagging and separator
  function Clean_Code (Code : Code_Type) return String is
    Res : String (1 .. Code'Length);
    Ind : Natural := 0;
  begin
    for C of Code loop
      if C /= Sep and then C /= Pad then
        Ind := Ind + 1;
        Res (Ind) := C;
      end if;
    end loop;
    return Res (1 .. Ind);
  end Clean_Code;

  -- Local: Lenght of Clean_Code
  function Clean_Length (Code : Code_Type) return Natural is
    (Clean_Code (Code)'Length);

  -- Precision (number of digits) can be 2, 4, 6, 8, 10, 11, 12, 13, 14, 15
  -- subtype Precision_Range is Positive range 2 .. 15;

  -- Default_Precision : constant Precision_Range := 10;

  -- Open Location Code (full)
  -- Notes: codes with less than 8 digits are padded with pairs of '0'
  --        and a '+' is inserted after the 8th digit
  -- The possible code lengths are 9, 11, 12 .. 16
  -- subtype Code_Length is Positive range 9 .. 16;

  -- subtype Code_Type is String;

  ----------------------
  -- Basic operations --
  ----------------------
  -- Encode a coordinate into a Open Location Code of a given precision
  function Encode (Coord : Coordinate;
                   Precision : Precision_Range := Default_Precision)
           return Code_Type is
    Lat, Lon : Real;
    Ilat, Ilon : Inte;
    Pos : Natural;
    Lat_Dig, Lon_Dig, Ind : Natural;
    Res : String (1 .. Code_Length'Last) := (others => ' ');
  begin
    Init_Logger;
    Logger.Log_Debug ("Encoding Lat: " & Coord.Lat'Img
                  & "  Lon: " & Coord.Lon'Img
                  & "  Precision: " & Precision'Img);
    Lat := Adjust_Lat (Coord.Lat, Precision);
    Lon := Normalize_Lon (Coord.Lon);
    Logger.Log_Debug ("  Adj Lat: " & Lat'Img & "  Lon: " & Lon'Img);

    -- This approach converts each value to an integer after multiplying it by
    --  the final precision. This allows us to use only integer operations, so
    --  avoiding any accumulation of floating point representation errors.

    -- Multiply values by their precision and convert to positive without any
    --  floating point operations
    -- Convert to int at the last stage to prserve precision
    Ilat := Inte (Max_Lat) * Grid_Lat_Precision_Inverse;
    Ilon := Inte (Max_Lon) * Grid_Lon_Precision_Inverse;
    declare
      Rlat : constant Real
           := Real (Ilat) + Lat * Real (Grid_Lat_Precision_Inverse);
      Rlon : constant Real
           := Real (Ilon) + Lon * Real (Grid_Lon_Precision_Inverse);
    begin
      Logger.Log_Debug ("  Reals Lat: " & Rlat'Img & "  Lon: " & Rlon'Img);
    end;
    Ilat := Trunc (Real (Ilat) + Lat * Real (Grid_Lat_Precision_Inverse));
    Ilon := Trunc (Real (Ilon) + Lon * Real (Grid_Lon_Precision_Inverse));
    Logger.Log_Debug ("  Ints Lat: " & Ilat'Img & "  Lon: " & Ilon'Img);

    -- Compute the full grid part of the code if necessary
    if Precision > Natural (Pair_Precision) then
      Logger.Log_Debug ("  In grid");
      Pos := Code_Length'Last;
      for I in 1 .. Grid_Precision loop
        Lat_Dig := Natural (Ilat rem Grid_Rows);
        Logger.Log_Debug ("    Lat dig: " & Lat_Dig'Img);
        Lon_Dig := Natural (Ilon rem Grid_Cols);
        Logger.Log_Debug ("    Lon dig: " & Lon_Dig'Img);
        Ind := Lat_Dig * Natural (Grid_Cols) + Lon_Dig;
        Logger.Log_Debug ("    Ind: " & Ind'Img);
        Res(Pos) := Alphabet (Ind + 1);
        Logger.Log_Debug ("    Letter: " & Res(Pos));
        Pos := Pos - 1;
        Ilat := Ilat / Grid_Rows;
        Ilon := Ilon / Grid_Cols;
      end loop;
    else
      Ilat := Ilat /  Grid_Rows ** Grid_Precision;
      Ilon := Ilon /  Grid_Cols ** Grid_Precision;
    end if;

    Logger.Log_Debug ("  In pairs");
    Pos := Code_Length'First + 1;
    -- Compute the pair section of the code in 10 .. 1
    for I in 1 .. Pair_Precision / 2 loop
      -- Lon character
      Ind := Natural (Ilon rem Base);
      Logger.Log_Debug ("    Lon ind: " & Ind'Img);
      Res(Pos) := Alphabet(Ind + 1);
      Logger.Log_Debug ("    Letter : " & Res(Pos));
      Pos := Pos - 1;
      Ilon := Ilon / Base;
      -- Lat character
      Ind := Natural (Ilat rem Base);
      Logger.Log_Debug ("    Lat ind: " & Ind'Img);
      Res(Pos) := Alphabet(Ind + 1);
      Logger.Log_Debug ("    Letter : " & Res(Pos));
      Pos := Pos - 1;
      Ilat := Ilat / Base;
    end loop;

    -- Insert the separator character (shift last 2 characters right by 1)
    Logger.Log_Debug ("  Raw code: " & Res);
    Res(Sep_Pos + 1 .. Sep_Pos + 2) := Res(Sep_Pos .. Sep_Pos + 1);
    Res(Sep_Pos) := Sep;
    Logger.Log_Debug ("Full code: " & Res);

    -- If we don't need to pad the code, return the requested section
    if Precision >= Natural (Pair_Precision) then
      return Res(1 .. Precision + 1);
    end if;

    -- Add the required padding characters.
    for I in Precision + 1 .. Sep_Pos - 1 loop
      Res(I) := Pad;
    end loop;
    -- Return the code up to and including the separator.
    return Res (1 .. Sep_Pos);
  end Encode;

  -- Verify the syntaxic validity of a code (code length and character set)
  function Is_Valid (Code : Code_Type) return Boolean is
     Sep_Index, Pad_Index : Natural;
  begin
    Init_Logger;
    Logger.Log_Debug ("Is valid " & Code);
    -- Minimum is <Sep><Lon><Lat> and Maximum is 8 <sep> 2 5
    if Code'Length < 3 or else Code'Length > Code_Length'Last then
      Logger.Log_Debug ("  Length: " & Code'Length'Img);
      return False;
    end if;
    -- Separator is required and unique
    Sep_Index := Locate (Code, Sep);
    if Sep_Index = 0 then
      Logger.Log_Debug ("  No separator");
      return False;
    end if;
    if Locate (Code, Sep, Sep_Index + 1) /= 0 then
      Logger.Log_Debug ("  Several separators");
      return False;
    end if;
    -- Check separator position (not after pos in full code and odd position)
    if Sep_Index > Sep_Pos or else Sep_Index rem 2 /= 1 then
      Logger.Log_Debug ("  Separator position: " & Sep_Index'Img);
      return False;
    end if;
    -- If there are characters after the separator, make sure there isn't just
    --  one of them (not legal).
    if Code'Length = Sep_Index + 1 then
      Logger.Log_Debug ("  Separator position: " & Sep_Index'Img
          & " versus length: " & Code'Length'Img);
      return False;
    end if;

    -- If padding before the separator, then it is even and separator is last
    Pad_Index := Locate (Code, Pad);
    if Pad_Index /= 0 then
      -- Short codes cannot have padding
      if Sep_Index < Sep_Pos then
        Logger.Log_Debug ("  Padding in short code");
        return False;
      end if;
      -- The first padding character needs to be in at odd position.
      if Pad_Index = 1 or else Pad_Index rem 2 /= 1 then
        Logger.Log_Debug ("  Padding at pos: " & Pad_Index'Img);
        return False;
      end if;
      -- Padded codes must not have anything after the separator
      if Code'Length > Sep_Index then
        Logger.Log_Debug ("  Padded and tail");
        return False;
      end if;
      declare
        Padded : constant String := Code (Pad_Index .. Sep_Index - 1);
        Padding : constant String (1 .. Padded'Length) := (others => Pad);
      begin
        if Padded /= Padding then
          Logger.Log_Debug ("  Wrong padding: " & Padded);
          return False;
        end if;
      end;
    end if;

    -- Are there any invalid characters
    for C of Code loop
      if C /= Sep and then C /= Pad and then Get_Index (C) = No_Index then
        Logger.Log_Debug ("  Invalid char: " & C);
        return False;
      end if;
    end loop;

   return True;

  end Is_Valid;

  -- Verify the full validity of a code (syntaxic and decodes to valid
  --  Coordinate
  -- Raises, if Code is invalid: Invalid_Code
  function Is_Full (Code : Code_Type) return Boolean is
    First_Lat, First_Lon : Inte;
  begin
    -- Must be valid
    if not Is_Valid (Code) then
      raise Invalid_Code;
    end if;
    -- If it's short, it's not full
    if Is_Short (Code) then
      return False;
    end if;
    -- Work out what the first latitude character indicates for latitude
    First_Lat := Inte (Get_Index (Code(Code'First)));
    First_Lat := First_Lat * Base;
    if Real (First_Lat) >= Max_Lat * 2.0 then
      -- The code would decode to a latitude of >= 90 degrees.
      return False;
    end if;
    -- Work out what the first longitude character indicates for longitude.
    First_Lon := Inte (Get_Index (Code(Natural'Succ(Code'First))));
    First_Lon := First_Lon * Base;
    if Real (First_Lon) >= Max_Lon * 2.0 then
      -- The code would decode to a longitude of >= 180 degrees.
      return False;
    end if;

    return True;
  end Is_Full;

  -- Decode a Open Location Code into two coordinates (south-west and
  --  north-east corner)
  -- Raises, if Code is invalid
  -- Invalid_Code : exception;
  procedure Decode (Code : in Code_Type;
                    South_West, North_East: out Coordinate) is
    -- This also checks the validity
    Letters : constant String := Clean_Code (Code);
    -- Int Lat and Lon
    Norm_Lat, Norm_Lon, Extr_Lat, Extr_Lon : Inte;
    Precision : Precision_Range;
    Lat_Prec, Lon_Prec : Real;
    Position, Row_Pos, Col_Pos, Row, Col : Inte;
    Ind : Natural;
    Index : Index_Range;
  begin
    Init_Logger;
    Logger.Log_Debug ("Decoding: " & Code);
    Logger.Log_Debug ("  Stripped: " & Letters);

    -- Code must be a valid full code
    if not Is_Full (Code) then
      raise Invalid_Code;
    end if;

    -- Process the pairs
    -- How many digits do we have to process
    Logger.Log_Debug ("  In pairs");
    if Letters'Length > Natural (Pair_Precision) then
      Precision := Precision_Range (Pair_Precision);
    else
      Precision := Letters'Length;
    end if;
    -- Initialise the values for each section. We work them out as integers and
    -- convert them to floats at the end
    --## rule off Parentheses
    Norm_Lat := -(Trunc (Max_Lat) * Pair_Precision_Inverse);
    Norm_Lon := -(Trunc (Max_Lon) * Pair_Precision_Inverse);
    --## rule on Parentheses
    Extr_Lat := 0;
    Extr_Lon := 0;
    -- Define the place value for the most significant pair
    Position := Base ** (Pair_Precision / 2 - 1);
    Ind := 1;
    while Ind < Precision loop
      Norm_Lat := Norm_Lat + Inte (Get_Index (Letters(Ind)))     * Position;
      Norm_Lon := Norm_Lon + Inte (Get_Index (Letters(Ind + 1))) * Position;
      if Ind < Precision - 1 then
        Position := Position / Base;
      end if;
      Logger.Log_Debug ("    Ind: " & Ind'Img & " Norm: " & Norm_Lat'Img & " " & Norm_Lon'Img);
      Ind := Ind + 2;
    end loop;
    -- Convert the place value to a float in degrees
    Lat_Prec := Real (Position) / Real (Pair_Precision_Inverse);
    Lon_Prec := Real (Position) / Real (Pair_Precision_Inverse);
    Logger.Log_Debug ("    Prec: " & Lat_Prec'Img & " " & Lon_Prec'Img);

     -- Process any extra precision digits
    Logger.Log_Debug ("  In grid");
    if Letters'Length > Natural (Pair_Precision) then
      Row_Pos := Grid_Rows ** (Grid_Precision - 1);
      Col_Pos := Grid_Cols ** (Grid_Precision - 1);
      for I in Natural (Pair_Precision) + 1 .. Letters'Last loop
        Index := Get_Index (Letters(I));
        Row := Inte (Index) / Grid_Cols;
        Col := Inte (Index) rem Grid_Cols;
        Extr_Lat := Extr_Lat + Row * Row_Pos;
        Extr_Lon := Extr_Lon + Col * Col_Pos;
        if I < Letters'Last then
          Row_Pos := Row_Pos / Grid_Rows;
          Col_Pos := Col_Pos / Grid_Cols;
        end if;
        Logger.Log_Debug ("    I: " & I'Img & " Index: " & Index'Img);
        Logger.Log_Debug ("    Row Col: " & Row'Img & " " & Col'Img);
        Logger.Log_Debug ("    Extra: " & Extr_Lat'Img & " " & Extr_Lon'Img);
        Logger.Log_Debug ("    Pos: " &  Row_Pos'Img & " " & Col_Pos'Img);
      end loop;
      -- Adjust the precisions from the integer values to degrees
      Lat_Prec := Real (Row_Pos) / Real (Grid_Lat_Precision_Inverse);
      Lon_Prec := Real (Col_Pos) / Real (Grid_Lon_Precision_Inverse);
    end if;
    Logger.Log_Debug ("  Prec: " & Lat_Prec'Img & " " & Lon_Prec'Img);

    -- Merge the values from the normal and extra precision parts of the code
    South_West.Lat := Real (Norm_Lat) / Real (Pair_Precision_Inverse)
                    + Real (Extr_Lat) / Real (Grid_Lat_Precision_Inverse);
    South_West.Lon := Real (Norm_Lon) / Real (Pair_Precision_Inverse)
                    + Real (Extr_Lon) / Real (Grid_Lon_Precision_Inverse);
    North_East.Lat := South_West.Lat + Lat_Prec;
    North_East.Lon := South_West.Lon + Lon_Prec;
  end Decode;

  -- Return the center of two coordinates
  function Center_Of (South_West, North_East: Coordinate) return Coordinate is
    Lat, Lon : Real;
  begin
    Lat := South_West.Lat + (North_East.Lat - South_West.Lat) / 2.0;
    if Lat > Max_Lat then
      Lat := Max_Lat;
    end if;
    Lon := South_West.Lon + (North_East.Lon - South_West.Lon) / 2.0;
    if Lon > Max_Lon then
      Lon := Max_Lon;
    end if;
    return (Lat, Lon);
  end Center_Of;

  -- Return the precision of a code (full or short)
  function Precision_Of (Code : Code_Type) return Precision_Range is
     Sep_Index : constant Natural := Locate (Code, Sep);
     Pad_Index : constant Natural := Locate (Code, Pad);
  begin
    -- Must be valid
    if not Is_Valid (Code) then
      raise Invalid_Code;
    end if;
    -- Code is XxYy "+" Zz <prec>   or   XxYy <padding> "+"
    if Sep_Index /= Code'Length then
      -- After Sep, 2 letters then extra precision (above 10)
      return Natural (Pair_Precision) + Code'Length - Sep_Index - 2;
    elsif Pad_Index = 0 then
      -- No padding
      return Sep_Pos - 1;
    else
      return Pad_Index - 1;
    end if;
  end Precision_Of;

  -----------------
  -- Short codes --
  -----------------
  -- Shorten the code (remove 2, 4 or 6 first digits) so that the resulting code
  --  is the closest matching code to the passed location
  -- Return the input code if it cannot be shorten
  -- Raises, if Code is invalid: Invalid_Code
  -- type Factor_Range is new Positive range 1 .. 3;
  function Shorten (Code : Code_Type;
                    Reference : Coordinate) return Code_Type is
    Sw, Ne, Cc : Coordinate;
    Lat, Lon, Dist, D2, Area_Edge : Real;
    Result : String (1 .. Code'Length);
    Safety_Factor : constant := 0.3;
    Removable_Length : Positive;
  begin
    Init_Logger;
    Logger.Log_Debug ("Shortening: " & Code
                   &  " to Lat: " & Reference.Lat'Img
                   & "  Lon: " & Reference.Lon'Img);
    -- Code must be a valid full code
    if not Is_Full (Code) then
      raise Invalid_Code;
    end if;
    -- Padded codes cannnot be shorten
    if Locate (Code, Pad) /= 0 then
      return Code;
    end if;
    -- Decode and find center
    Decode (Code, Sw, Ne);
    Cc := Center_Of (Sw, Ne);
    Logger.Log_Debug ("  Center Lat: " & Cc.Lat'Img
                    & "  Lon: " & Cc.Lon'Img);
    -- Ensure that latitude and longitude are valid
    Lat := Adjust_Lat (Reference.Lat, Precision_Of (Code));
    Lon := Normalize_Lon (Reference.Lon);
    -- How close are the latitude and longitude to the code center
    Dist := abs (Cc.Lat - Lat);
    D2 := abs (Cc.Lon - Lon);
    if Dist < D2 then
      Dist := D2;
    end if;
    Logger.Log_Debug ("  Dist: " & Dist'Img);

    -- Shorten if possible
    Result := Code;
    for I in 1 .. 3 loop
       Removable_Length := Positive (Pair_Precision - 2 * Inte (I));
       Area_Edge := Precision_Of (Removable_Length) * Safety_Factor;
       Logger.Log_Debug ("    Area edge: " & Area_Edge'Img);
       if Dist < Area_Edge then
         return Result (Removable_Length + 1 .. Result'Last);
       end if;
     end loop;
     return Result;
  end Shorten;

  -- Verify the syntaxic validity of a short code, i.e. a full code, possibliy
  -- with extra precision (more than 2 digits after the '+') without the
  -- 2, 4 or 6 first digits
  -- Returns False for a full code
  -- Raises, if Code is invalid: Invalid_Code
  function Is_Short (Code : Code_Type) return Boolean is
  begin
    -- Must be valid
    if not Is_Valid (Code) then
      raise Invalid_Code;
    end if;
    -- Short if there are less characters than expected before the separator
    return Locate (Code, Sep) < Sep_Pos;
  end Is_Short;

  -- Return the nearest full code matching the provided short code and location
  -- Raises, if Code is invalid or not short: Invalid_Code
  function Nearest (Code : Code_Type; Reference : Coordinate)
           return Code_Type is
    Lat, Lon, Resolution, Half_Res : Real;
    Padding_Len : Natural;
    Padding_Code : Code_Type (1 .. Default_Precision + 1);
    -- This checks the validity of Code
    Padded_Code : Code_Type (1 .. Precision_Of (Code) + 1);
    Sw, Ne, Ce : Coordinate;
  begin
    Init_Logger;
    Logger.Log_Debug ("Nearest of: " & Code
                   &  " to Lat: " & Reference.Lat'Img
                   & "  Lon: " & Reference.Lon'Img);
    -- Full code => To upper
    if not Is_Short (Code) then
      return Upper_Str (Code);
    end if;
    -- Ensure that latitude and longitude are valid
    Lat := Adjust_Lat (Reference.Lat, Precision_Of (Code));
    Lon := Normalize_Lon (Reference.Lon);

    -- Compute the number of digits we need to recover
    Padding_Len := Sep_Pos - Locate (Code, Sep) ;
    Logger.Log_Debug ("Padding len: " & Padding_Len'Img);
    -- The resolution (height and width) of the padded area in degrees
    Resolution := Real (Base) ** (2 - Padding_Len / 2);
    Logger.Log_Debug ("Resolution: " & Resolution'Img);
    -- Distance from the center to an edge (in degrees)
    Half_Res := Resolution / 2.0;
    -- Use the reference location to pad the supplied short code and decode it
    Padding_Code := Encode ( (Lat, Lon) );
    Logger.Log_Debug ("Padding code: " & Padding_Code);
    Padded_Code(1 .. Padding_Len) := Padding_Code (1 .. Padding_Len);
    Padded_Code(Padding_Len + 1 .. Padded_Code'Last) := Code;
    Logger.Log_Debug ("Padded code: " & Padded_Code);
    Decode (Padded_Code, Sw, Ne);

    -- How many degrees latitude is the code from the reference? If it is more
    -- than half the resolution, we need to move it north or south but keep it
    -- within -90 to 90 degrees.
    Ce := Center_Of (Sw, Ne);
    if Lat + Half_Res < Ce.Lat
    and then Ce.Lat - Resolution > -Max_Lat then
      Ce.Lat := Ce.Lat - Resolution;
    elsif Lat - Half_Res > Ce.Lat
    and then Ce.Lat + Resolution < Max_Lat then
      Ce.Lat := Ce.Lat + Resolution;
    end if;

    -- How many degrees longitude is the code from the reference
    if Lon + Half_Res < Ce.Lon then
      Ce.Lon := Ce.Lon - Resolution;
    elsif Lon - Half_Res > Ce.Lon then
      Ce.Lon := Ce.Lon + Resolution;
    end if;

    -- Encode the new reference
    return Encode(Ce, Clean_Length (Code) + Padding_Len);
  end Nearest;

end Olc;

