with Ada.Text_Io;
with Argument, Sys_Calls;
with Conv, Lat_Lon, String_Util, Great_Circle;

procedure T_Gc is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
      & " add.mm.ss/oddd.mm.ss add.mm.ss/oddd.mm.ss");
    Ada.Text_Io.Put_Line (" where a is N or S and o is E or W.");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;

  A, B : Lat_Lon.Lat_Lon_Geo_Rec;
  Heading  : Conv.Geo_Coord_Rec;
  Distance : Lat_Lon.Distance;

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  -- Convert args in lat_lon of A and B
  begin
    A := String_Util.Str2Geo(Argument.Get_Parameter(1));
    B := String_Util.Str2Geo(Argument.Get_Parameter(2));
  exception
    when others =>
      Usage;
      return;
  end;

  Great_Circle.Compute_Route(A, B, Heading, Distance);

  Ada.Text_Io.Put_Line ("Route is " & String_Util.Angle2Str(Heading));
  Ada.Text_Io.Put_Line ("Distance is " & String_Util.Dist2Str(Distance));

end T_Gc;

