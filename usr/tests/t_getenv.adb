with My_Io;
with Sys_Calls;
with Argument;
procedure T_Getenv is

  Set : Boolean;
  Tru : Boolean;
  Res : String (1..80);
  Len : Natural;

begin
  for I in 1 .. Sys_Calls.Environ_Len loop
    My_Io.Put_Line (Sys_Calls.Environ_Val(I));
  end loop;

  if Argument.Get_Nbre_Arg = 0 then
    My_Io.Put ("GETENV");
    Sys_Calls.Getenv ("GETENV", Set, Tru, Res, Len);
  else
    My_Io.Put (Argument.Get_Parameter);
    Sys_Calls.Getenv (Argument.Get_Parameter, Set, Tru, Res, Len);
  end if;
  if not Set then
    My_Io.Put_Line (" Not set");
  else
    My_Io.Put (" ->" & Res (1 .. Len) & "<");
    if Tru then
      My_Io.Put (" truncated");
    end if;
    My_Io.New_Line;
  end if;
end T_Getenv;
