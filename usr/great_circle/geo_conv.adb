with As.U.Utils, Argument, Basic_Proc, Directory, My_Math, Reg_Exp,
     Str_Util.Regex, Normalization, Int_Img;
with Units, Lat_Lon, String_Util;
procedure Geo_Conv is

  -- Display help
  procedure Help is
  begin
    Basic_Proc.Put_Line_Output ("Usage: "
        & Directory.Basename (Argument.Get_Program_Name)
        & " <location> [ <precisions> ]");
    Basic_Proc.Put_Line_Output (
        "<location> ::= <sexa> | <deci> | code>");
    Basic_Proc.Put_Line_Output (
        "<sexa>     ::= add.mm.ssss[ss]/oddd.mm.ssss[ss]");
    Basic_Proc.Put_Line_Output (
        "  where a is N or S and o is E or W.");
    Basic_Proc.Put_Line_Output (
        "<deci>     ::= D.d/D.d");
    Basic_Proc.Put_Line_Output (
        "  where D is positivie or negative, and d up to 9 digits");
    Basic_Proc.Put_Line_Output (
        "<code> ::= <map_code> | <olc_code> | <geohash36> | <geohash>");
    Basic_Proc.Put_Line_Output (
        "<map_code> ::= [<context>:]<mapcode>");
    Basic_Proc.Put_Line_Output (
        "<olc_code>     ::= <open_location_code>");
    Basic_Proc.Put_Line_Output (
        "<precisions>   ::= -p <prec_set>");
    Basic_Proc.Put_Line_Output (
        "<prec_set>     ::= "
      & "[<sexa_prec>]/[<deci_prec>]/[<map_prec>]/[<olc_prec>]");
    Basic_Proc.Put_Line_Output (
        "                   /[<gh36_prec>]/[<gh_prec>]");
    Basic_Proc.Put_Line_Output ("<sexa_prec>    ::= 4 | 6");
    Basic_Proc.Put_Line_Output ("<deci_prec>    ::= 1 .. 9");
    Basic_Proc.Put_Line_Output ("<map_prec>     ::= 0 .. 8");
    Basic_Proc.Put_Line_Output ("<olc_prec>     ::= 2 | 15");
    Basic_Proc.Put_Line_Output ("<gh36_prec>    ::= 1 | 15");
    Basic_Proc.Put_Line_Output ("<gh_prec>      ::= 1 | 12");
    Basic_Proc.Put_Line_Output (
        "  where the default precisions are the max of each kind.");
  end Help;

  -- DIsplay error message
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg);
    Help;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Strip the potential tail (@...) of a Geohash36 or Geohash code
  function Strip (Code : String) return String is
    Index : constant Natural := Str_Util.Locate (Code, "@");
  begin
    if Index = 0 then
      return Code;
    else
      return Code (Code'First .. Index - 1);
    end if;
  end Strip;

  -- Argument
  Arg : As.U.Asu_Us;

  -- Option: Precision array
  Opt : As.U.Asu_Us;
  Precisions : As.U.Utils.Asu_Ua.Unb_Array;

  -- Kind of input
  type In_Kind_List is (Sexa, Deci, Map, Olc, Gh36, Gh);
  Kind : In_Kind_List;
  Found : Boolean;

  -- Pattern for each kind of input
  use all type As.U.Asu_Us;
  Patterns : constant array (In_Kind_List) of As.U.Asu_Us := (
    -- Fixed size DD.MM.SSssss[ss]/DDD.MM.SSssss[ss],  Ex: N23.41.3225/E001.21.3456
    Sexa => Tus ("[NnSs][0-9]{2}\.[0-9]{2}\.[0-9]{4,6}/[EeWw][0-9]{3}\.[0-9]{2}\.[0-9]{4,6}"),
    -- Variable int and frac parts (up to 9) Ex: 42.123456789/-123.987654321
    Deci => Tus ("\-?[0-9]{1,2}\.[0-9]{1,9}/-?[0-9]{1,3}\.[0-9]{1,9}"),
    -- [Territory:]code, code with at least a dot and a letter
    Map => Tus ("([A-Z-]+:)?(.*[A-Z].*\..+|.+\..*[A-Z].*)"),
    -- Letter or num and a "+"
    Olc => Tus ("[A-Z0-9]+\+[A-Z0-9]*"),
    -- Gh36 alphabet and optional indicator
    Gh36 => Tus ("[23456789bBCdDFgGhHjJKlLMnNPqQrRtTVWX]+(@GH36)?"),
    -- Gh alphabet and optional indicator
    Gh => Tus ("[0123456789bcdefghjkmnpqrstuvwxyz]+(@GH)?") );

  -- Index of '/' in input
  Slash : Natural;
  -- Signed lat lon
  Point : Lat_Lon.Signed_Deg_Rec;

  -- Default precision of outputs (seconds, fraction of degrees, mapcode, olc, geohash)
  -- Default is the max
  -- Seconds: 4 or 6
  Sec_Len : Positive := 6;
  -- Fractions of degrees: 1 to 9
  Frac_Len : Positive := 9;
  Map_Precision : Lat_Lon.Map_Precisions := Lat_Lon.Map_Precisions'Last;
  Olc_Precision : Lat_Lon.Olc_Precisions := Lat_Lon.Olc_Precisions'Last;
  Gh36_Precision : Lat_Lon.Gh36_Precisions := Lat_Lon.Gh36_Precisions'Last;
  Gh_Precision : Lat_Lon.Gh_Precisions := Lat_Lon.Gh_Precisions'Last;

  -- Parse a precision
  function Parse_Prec (Idx : Positive; Default : Natural) return Natural is
  begin
    if Precisions.Element(Idx).Is_Null then
      return Default;
    end if;
    return Natural'Value (Precisions.Element(Idx).Image);
  end Parse_Prec;

  -- Format output in degrees
  function Format_Degree (D : in Units.Degree) return String is
  begin
    return Str_Util.Strip (
       Normalization.Normal_Fixed (My_Math.Real (D), Frac_Len + 5, 4, '0'),
                                   From => Str_Util.Head);
  end Format_Degree;

begin
  -- Check args
  if Argument.Get_Nbre_Arg /= 1
  and then Argument.Get_Nbre_Arg /= 3 then
    Error ("One and only one argument expected, with one potential precision");
    return;
  end if;

  -- Parse args
  Argument.Get_Parameter (Arg);
  -- Optional set of precisions
  if Argument.Get_Nbre_Arg = 3 then
    if Argument.Get_Parameter (Occurence => 2) /= "-p" then
      Error ("Invalid argument " & Arg.Image);
      return;
    end if;
    Argument.Get_Parameter (Opt, Occurence => 3);
    Precisions := As.U.Utils.Asu_Ua.To_Unbounded_Array (
        Str_Util.Regex.Split_Sep (Opt.Image, "/") );
    if Precisions.Length /= 6 then
      Error ("Invalid argument " & Opt.Image);
      return;
    end if;
    -- Seconds of degrees
    Sec_Len := Parse_Prec (1, Sec_Len);
    if Sec_Len /= 4 and then Sec_Len /= 6 then
      Error ("Invalid precision " & Int_Img (Sec_Len));
      return;
    end if;
    -- Fractions of degrees
    Frac_Len := Parse_Prec (2, Frac_Len);
    if Frac_Len > 9 then
      Error ("Invalid precision " & Int_Img (Frac_Len));
      return;
    end if;
    -- Mapcode precision
    Map_Precision :=
        Lat_Lon.Map_Precisions (Parse_Prec (3, Positive (Map_Precision)));
    -- Olc precision
    Olc_Precision :=
        Lat_Lon.Olc_Precisions (Parse_Prec (4, Positive (Olc_Precision)));
    -- Gh36 precision
    Gh36_Precision :=
        Lat_Lon.Gh36_Precisions (Parse_Prec (5, Positive (Gh36_Precision)));
    -- Gh precision
    Gh_Precision :=
        Lat_Lon.Gh_Precisions (Parse_Prec (6, Positive (Gh_Precision)));
  end if;

  -- Check syntax of argument
  Found := False;
  for K in In_Kind_List loop
    if Reg_Exp.Match (Patterns(K).Image, Arg.Image, True) then
      Kind := K;
      Found := True;
      exit;
    end if;
  end loop;
  if not Found then
    Error ("Invalid argument " & Arg.Image);
    return;
  end if;
  if Kind = Gh36 or else Kind = Gh then
    if Reg_Exp.Match (Patterns(Gh36).Image, Arg.Image, True)
    and then Reg_Exp.Match (Patterns(Gh).Image, Arg.Image, True) then
      Error ("Ambibuous argument (geohash36 or geohash code)");
      return;
    end if;
    Arg := As.U.Tus (Strip (Arg.Image));
  end if;

  -- Set Point
  case Kind is
    when Sexa =>
      if Arg.Length = String_Util.Geo_Str'Length then
        Point := Lat_Lon.Geo2Sig (String_Util.Str2Geo(Arg.Image));
      elsif Arg.Length = String_Util.Geo_Lstr'Length then
        Point := Lat_Lon.Geo2Sig (String_Util.Lstr2Geo(Arg.Image));
      else
        Error ("Invalid argument " & Arg.Image);
        return;
      end if;
    when Deci =>
      Slash := Str_Util.Locate (Arg.Image, "/");
      Point := (
         Lat => Units.Degree'Value (Arg.Slice (1, Slash - 1)),
         Lon => Units.Degree'Value (Arg.Slice (Slash + 1, Arg.Length)) );
    when Map =>
      Point := Lat_Lon.Mapcode2Deg (Arg.Image);
    when Olc =>
      Point := Lat_Lon.Olc2Deg (Arg.Image);
    when Gh36 =>
      Point := Lat_Lon.Gh362Deg (Arg.Image);
    when Gh =>
      Point := Lat_Lon.Gh2Deg (Arg.Image);
   end case;

   -- Display the 4 values
   -- Sexa,
   if Sec_Len = 4 then
     Basic_Proc.Put_Line_Output (String_Util.Geo2Str (Lat_Lon.Sig2Geo (Point)));
   else
     Basic_Proc.Put_Line_Output (String_Util.Geo2Lstr (Lat_Lon.Sig2Geo (Point)));
   end if;
   -- Deci
   Basic_Proc.Put_Line_Output (Format_Degree (Point.Lat)
                       & "/" & Format_Degree (Point.Lon));
  -- Mapcode
  Basic_Proc.Put_Line_Output (Lat_Lon.Deg2Mapcode (Point, Map_Precision));
  -- Open Location Code
  Basic_Proc.Put_Line_Output (Lat_Lon.Deg2Olc (Point, Olc_Precision));
  -- Geohash36 Code
  Basic_Proc.Put_Line_Output (Lat_Lon.Deg2Gh36 (Point, Gh36_Precision));
  -- Geohash Code
  Basic_Proc.Put_Line_Output (Lat_Lon.Deg2Gh (Point, Gh_Precision));

end Geo_Conv;

