with MY_IO;
with ARGUMENT;

with COMMON;
with ACTION;


procedure MMIND is
begin
  declare
    LEVEL : COMMON.LAST_LEVEL_RANGE;
  begin
    if ARGUMENT.GET_NBRE_ARG > 1 then
      raise CONSTRAINT_ERROR;
    end if;
    LEVEL := COMMON.LAST_LEVEL_RANGE'VALUE (ARGUMENT.GET_PARAMETER);
    COMMON.SET_LEVEL (LEVEL);
  exception
    when ARGUMENT.ARGUMENT_NOT_FOUND =>
      LEVEL := COMMON.LAST_LEVEL_RANGE'FIRST;
      COMMON.SET_LEVEL (LEVEL);
    when CONSTRAINT_ERROR =>
      MY_IO.PUT_LINE (
       "Syntax ERROR. Usage is ""MMIND [ <level> ]"" (level from 3 to 5).");
      return;
  end;

  ACTION.INIT;

  loop
    exit when not ACTION.PLAY;
  end loop;

exception
  when ACTION.NO_MOUSE =>
    MY_IO.PUT_LINE (
     "Sorry, MOUSE not found.");
    return;

end MMIND;