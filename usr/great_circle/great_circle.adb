with Ada.Text_Io;
with Argument, Sys_Calls, C_Nbres;
with Conv, Lat_Lon, String_Util;

procedure Great_Circle is

  -- Lat lon of A and B in rad
  Cra, Crb : Lat_Lon.Lat_Lon_Rad_Rec;
  -- Delta lon
  Dcr : Lat_Lon.Lat_Lon_Rad_Rec;

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
      & " add.mm.ss/oddd.mm.ss add.mm.ss/oddd.mm.ss");
    Ada.Text_Io.Put_Line (" where a is N or S and o is E or W.");
    Sys_Calls.Set_Error_Exit_Code;
  end Usage;

begin

  if Argument.Get_Nbre_Arg /= 2 then
    Usage;
    return;
  end if;

  -- Convert args in lat_lon
  begin
    Cra := Lat_Lon.Geo2Rad(String_Util.Str2Geo(Argument.Get_Parameter(1)));
    Crb := Lat_Lon.Geo2Rad(String_Util.Str2Geo(Argument.Get_Parameter(2)));
  exception
    when others =>
      Usage;
      return;
  end;

  -- Compute alpha and beta
  declare
    use C_Nbres;
  begin
    Dcr := (X => Crb.X - Cra.X, Y => Crb.Y - Cra.Y);
  end;

end Great_Circle;

