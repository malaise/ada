with Ada.Text_Io;
with Argument, Num_Match;

procedure T_Num_Match is

  procedure Usage is
  begin
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
       & " <Natural> [ <Criteria_String> ]");
  end Usage;

  Str : String(1 .. 256);
  Len : Natural;
  Num : Natural;
  Res : Boolean;
  pragma Unreferenced (Res);
begin

  if Argument.Get_Nbre_Arg = 1 then
    Num := Natural'Value (Argument.Get_Parameter);
    Len := 0;
  elsif Argument.Get_Nbre_Arg = 2 then
    Num := Natural'Value (Argument.Get_Parameter (Occurence => 1));
    Argument.Get_Parameter (Str, Len, 2);
  else
    Usage;
    return;
  end if;

  Res := Num_Match (Num, Str(1 .. Len));

  Ada.Text_Io.Put (Num'Img);
  if Num_Match (Num, Str(1 .. Len)) then
    Ada.Text_Io.Put (" matches");
  else
    Ada.Text_Io.Put (" does not match");
  end if;
  Ada.Text_Io.Put_Line (" >" & Str(1 .. Len) & "<");

exception
  when Argument.Argument_Not_Found | Argument.Argument_Too_Long =>
    Usage;
end T_Num_Match;

