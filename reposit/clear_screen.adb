with SYS_CALLS;
procedure CLEAR_SCREEN is
  COMMAND : constant STRING := "clear";
  RES : INTEGER;
begin
  RES := SYS_CALLS.CALL_SYSTEM (COMMAND);
end CLEAR_SCREEN;

