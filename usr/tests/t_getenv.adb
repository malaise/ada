with MY_IO;
with SYS_CALLS;
with ARGUMENT;
procedure T_GETENV is

  SET : BOOLEAN;
  TRU : BOOLEAN;
  RES : STRING (1..5);
  LEN : NATURAL;

begin

  if ARGUMENT.GET_NBRE_ARG = 0 then
    SYS_CALLS.GETENV ("GETENV", SET, TRU, RES, LEN);
  else
    SYS_CALLS.GETENV (ARGUMENT.GET_PARAMETER(1), SET, TRU, RES, LEN);
  end if;
  if not SET then
    MY_IO.PUT_LINE ("Not set");
  else
     MY_IO.PUT_LINE (">" & RES (1 .. LEN) & "< truncated: " & BOOLEAN'IMAGE(TRU));
  end if;
end T_GETENV;
