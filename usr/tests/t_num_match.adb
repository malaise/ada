with Ada.Text_Io;
with Num_Match, My_Io;

procedure T_Num_Match is

  Str : String(1 .. 256);
  Len : Natural;
  Num : Natural;

begin

  Ada.Text_Io.Put ("Num: ");
  My_Io.Get (Num);
   Ada.Text_Io.skip_Line;

  Ada.Text_Io.Put ("Criteria: ");
  Ada.Text_Io.Get_Line (Str, Len);

  if Num_Match (Num, Str(1 .. Len)) then
    Ada.Text_Io.Put (Num'Img & " matches");
  else
    Ada.Text_Io.Put (Num'Img & " does not match");
  end if;

end T_Num_Match;

