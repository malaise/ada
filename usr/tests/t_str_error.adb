with My_Io, Sys_Calls, Normal;
procedure T_Str_Error is
begin

  for I in 1 .. 256 loop
    exit when Sys_Calls.Str_Error(I) = "";
    My_Io.Put_Line (Normal(I, 3) & " -> " & Sys_Calls.Str_Error(I) );
  end loop;

end T_Str_Error;
