with MY_IO, ARGUMENT, CON_IO, DOS;
with PERS_DEF, PERS_FIL, MESU_NAM, MESU_EDI, MESU_FIL;

procedure INS_MESU is
  FILE_NAME : MESU_NAM.FILE_NAME_STR;
  DONE : BOOLEAN;

  procedure ERROR (MSG : in STRING) is
  begin
    MY_IO.PUT_LINE (MSG);
    MY_IO.PUT_LINE ("USAGE : " & ARGUMENT.GET_PROGRAM_NAME
                               & " [ <mesure_file_name> ]");
  end ERROR;

begin

  if ARGUMENT.GET_NBRE_ARG = 1 then

    begin
      FILE_NAME := ARGUMENT.GET_PARAMETER;
    exception
      when others =>
        ERROR ("Invalid argument.");
        return;
    end;

  elsif ARGUMENT.GET_NBRE_ARG = 0 then
    FILE_NAME := (others => ' ');
  else
    ERROR ("Invalid argument.");
    return;
  end if;


  PERS_FIL.LOAD;


  MESU_EDI.EDIT (FILE_NAME, DONE);

  CON_IO.RESET_TERM;

exception
  when MESU_FIL.FILE_NOT_FOUND_ERROR =>
    CON_IO.RESET_TERM;
    raise;
  when others =>
    DOS.SOUND (3);
    delay 5.0;
    CON_IO.RESET_TERM;
    raise;
end INS_MESU;
