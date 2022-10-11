with As.U, Argument, Basic_Proc, Directory, My_Math, Reg_Exp,
     Str_Util, Normalization;
with Units, Lat_Lon, String_Util;
procedure Conv is

  -- Display help
  procedure Help is
  begin
    Basic_Proc.Put_Line_Output ("Usage: "
        & Directory.Basename (Argument.Get_Program_Name) & " <location>");
    Basic_Proc.Put_Line_Output (
        "<location> ::= <sexa> | <deci> | <map_code> | <ol_code>");
    Basic_Proc.Put_Line_Output (
        "<sexa>     ::= add.mm.ssss/oddd.mm.ssss");
    Basic_Proc.Put_Line_Output (
        "  where a is N or S and o is E or W.");
    Basic_Proc.Put_Line_Output (
        "<deci>     ::= D.d/D.d");
    Basic_Proc.Put_Line_Output (
        "  where i is positivie or degative, and d up to 9 digits");
    Basic_Proc.Put_Line_Output (
        "<map_code> ::= [<context>:]<mapcode>");
    Basic_Proc.Put_Line_Output (
        "<ol_code>      ::= <open_location_code>");
  end Help;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg);
    Help;
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Argument
  Arg : As.U.Asu_Us;

  -- Kind of input
  type In_Kind_List is (Sexa, Deci, Map, Olc);
  Kind : In_Kind_List;
  Found : Boolean;

  -- Pattern for each kind of input
  use all type As.U.Asu_Us;
  Patterns : constant array (In_Kind_List) of As.U.Asu_Us := (
    -- Fixed size DD.MM.SSssss/DDD.MM.SSssss,  Ex: N23.41.3225/E001.21.3456
    Sexa => Tus ("[NnSs][0-9]{2}\.[0-9]{2}\.[0-9]{4}/[EeWw][0-9]{3}\.[0-9]{2}\.[0-9]{4}"),
    -- Variable int and frac parts (up to 9) Ex: 42.123456789/-123.987654321
    Deci => Tus ("\-?[0-9]{1,2}\.[0-9]{1,9}/-?[0-9]{1,3}\.[0-9]{1,9}"),
    -- [Territory:]code, code with at least a dot and a letter
    Map => Tus ("([A-Z-]+:)?(.*[A-Z].*\..+|.+\..*[A-Z].*)"),
    -- Letter or num and a "+"
    Olc => Tus ("[A-Z0-9]+\+[A-Z0-9]*") );

  -- Index of '/' in input
  Slash : Natural;
  -- Lon (X) and lat (Y) in radian, from input
  Point : Lat_Lon.Lat_Lon_Rad_Rec;
  -- Same in signed degrees
  Degs : Lat_Lon.Deg_Rec;

  -- Precision of outputs (fraction of degrees, mapcode, olc)
  Frac_Len : constant := 9;
  Map_Precision : constant Lat_Lon.Map_Precisions := 8;
  Olc_Precision : constant Lat_Lon.Olc_Precisions := 15;

  -- Format output in degrees
  function Format_Degree (D : in Units.Degree) return String is
  begin
    return Str_Util.Strip (
       Normalization.Normal_Fixed (
          My_Math.Real (D), Frac_Len + 5, 4, '0'),
       From => Str_Util.Head);
  end Format_Degree;


begin
  if Argument.Get_Nbre_Arg /= 1 then
    Error ("One and only one argument expected");
    return;
  end if;
  Argument.Get_Parameter (Arg);

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

  -- Set Point in radians
  case Kind is
    when Sexa =>
      Point := Lat_Lon.Geo2Rad (String_Util.Str2Geo(Arg.Image));
    when Deci =>
      Slash := Str_Util.Locate (Arg.Image, "/");
      Point := Lat_Lon.Deg2Rad ( (
         Lat => Units.Degree'Value (Arg.Slice (1, Slash - 1)),
         Lon => Units.Degree'Value (Arg.Slice (Slash + 1, Arg.Length))) );
    when Map =>
      Point := Lat_Lon.Mapcode2Rad (Arg.Image);
    when Olc =>
      Point := Lat_Lon.Olc2Rad (Arg.Image);
   end case;

   -- Display the 4 values
   -- Sexa,
   Basic_Proc.Put_Line_Output (String_Util.Geo2Str (Lat_Lon.Rad2Geo (Point)));
   -- Deci
   Degs := Lat_Lon.Rad2Deg (Point);
   Basic_Proc.Put_Line_Output (Format_Degree (Degs.Lat)
                       & "/" & Format_Degree (Degs.Lon));
  -- Mapcode
  Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Mapcode (Point, Map_Precision));
  -- Open Location Code
  Basic_Proc.Put_Line_Output (Lat_Lon.Rad2Olc (Point, Olc_Precision));

end Conv;

