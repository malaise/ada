with TEXT_IO;
with ARGUMENT;
procedure BEEP is
  NB_BEEP : POSITIVE;
  subtype DELTA_BEEP_RANGE is DURATION range 0.01 .. 1.0;
  DELTA_BEEP : DELTA_BEEP_RANGE;
begin
  if ARGUMENT.GET_NBRE_ARG > 2 then
    raise CONSTRAINT_ERROR;
  end if;
  if ARGUMENT.GET_NBRE_ARG >= 1 then
    NB_BEEP := INTEGER'VALUE(ARGUMENT.GET_PARAMETER(OCCURENCE => 1));
  else
    NB_BEEP := 1;
  end if;
  if ARGUMENT.GET_NBRE_ARG >= 2 then
    DELTA_BEEP := DELTA_BEEP_RANGE'VALUE(ARGUMENT.GET_PARAMETER(OCCURENCE => 2));
  else
    DELTA_BEEP := 0.25;
  end if;
  for I in 1 .. NB_BEEP loop
    TEXT_IO.PUT (ASCII.BEL);
    if I /= NB_BEEP then
      delay DELTA_BEEP;
    end if;
  end loop;
exception
  when others =>
    TEXT_IO.PUT (ASCII.BEL);
end BEEP;
