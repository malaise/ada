with MY_IO, SYS_CALLS, NORMAL;
procedure T_STR_ERROR is
begin

  for I in 1 .. 256 loop
    exit when SYS_CALLS.STR_ERROR(I) = "";
    MY_IO.PUT_LINE (NORMAL(I, 3) & " -> " & SYS_CALLS.STR_ERROR(I) );
  end loop;

end T_STR_ERROR;
