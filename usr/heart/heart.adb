with TEXT_IO;
with CON_IO, DOS, ARGUMENT, X_MNG;
with PERS_FIL, MESU_MNG, STR_MNG;
procedure HEART is
  NB_MONTH : STR_MNG.OFFSET_RANGE;

  procedure END_OF_PROGRAM is
  begin
    CON_IO.RESET_TERM;
  end END_OF_PROGRAM;

begin
  begin
    if ARGUMENT.GET_NBRE_ARG = 0 then
      NB_MONTH := 0;
    elsif ARGUMENT.GET_NBRE_ARG = 1 then
      NB_MONTH := STR_MNG.OFFSET_RANGE'VALUE (ARGUMENT.GET_PARAMETER);
    else
      raise CONSTRAINT_ERROR;
    end if;
  exception
    when others =>
      TEXT_IO.PUT_LINE ("SYNTAX ERROR. Usage : "
                      & ARGUMENT.GET_PROGRAM_NAME & " [ <nb_month> ]");
      return;
  end;

  CON_IO.INIT;

  PERS_FIL.LOAD;
  MESU_MNG.LIST_MESURES (NB_MONTH);

  END_OF_PROGRAM;

exception
  when others =>
    DOS.SOUND (3);
    END_OF_PROGRAM;
    raise;
end HEART;
