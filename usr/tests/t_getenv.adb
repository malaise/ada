with MY_IO;
with SYS_CALLS;
procedure T_GETENV is

  SET : BOOLEAN;
  TRU : BOOLEAN;
  RES : STRING (1..5);
  LEN : NATURAL;

begin

  SYS_CALLS.GETENV ("GETENV", SET, TRU, RES, LEN);
  if not SET then
    MY_IO.PUT_LINE ("Not set");
  else
     MY_IO.PUT_LINE (">" & RES (1 .. LEN) & "< truncated: " & BOOLEAN'IMAGE(TRU));
  end if;
end T_GETENV;
