with My_Io;
with Sys_Calls;
with Argument;
procedure T_Getenv is

  Set : Boolean;
  Tru : Boolean;
  Res : String (1..5);
  Len : Natural;

begin

  if Argument.Get_Nbre_Arg = 0 then
    Sys_Calls.Getenv ("GETENV", Set, Tru, Res, Len);
  else
    Sys_Calls.Getenv (Argument.Get_Parameter(1), Set, Tru, Res, Len);
  end if;
  if not Set then
    My_Io.Put_Line ("Not set");
  else
     My_Io.Put_Line (">" & Res (1 .. Len) & "< truncated: " & Boolean'Image(Tru));
  end if;
end T_Getenv;
