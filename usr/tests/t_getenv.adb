with Basic_Proc, Sys_Calls, Argument;
procedure T_Getenv is

  Set : Boolean;
  Tru : Boolean;
  Res : String (1..80);
  Len : Natural;

begin
  for I in 1 .. Sys_Calls.Environ_Len loop
    Basic_Proc.Put_Line_Output (Sys_Calls.Environ_Val(I));
  end loop;

  if Argument.Get_Nbre_Arg = 0 then
    Basic_Proc.Put_Output ("GETENV");
    Sys_Calls.Getenv ("GETENV", Set, Tru, Res, Len);
  else
    Basic_Proc.Put_Output (Argument.Get_Parameter);
    Sys_Calls.Getenv (Argument.Get_Parameter, Set, Tru, Res, Len);
  end if;
  if not Set then
    Sys_Calls.Set_Error_Exit_Code;
    Basic_Proc.Put_Line_Output (" Not set");
  else
    Basic_Proc.Put_Output (" ->" & Res (1 .. Len) & "<");
    if Tru then
      Basic_Proc.Put_Output (" truncated");
      Sys_Calls.Set_Error_Exit_Code;
    end if;
    Basic_Proc.New_Line_Output;
  end if;
end T_Getenv;

