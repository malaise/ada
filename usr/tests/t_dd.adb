with TEXT_IO;
with ARGUMENT;
with DYN_DATA;

procedure T_DD is

  subtype DATA is STRING (1 .. 1024);
  type DATA_ACCESS is access DATA;

  package MY_DYN_DATA is new DYN_DATA (DATA, DATA_ACCESS);

  MAX_DATA : constant := 256;

  DATA_ACCESS_ARRAY : array (1 .. MAX_DATA) of DATA_ACCESS;

  NB_LOOP : NATURAL;

  ABORT_EXCEPTION : exception;

  procedure ONE_CYCLE is
  begin
    for J in 1 .. MAX_DATA loop
      DATA_ACCESS_ARRAY(J) := MY_DYN_DATA.ALLOCATE;
    end loop;

    for J in 1 .. MAX_DATA loop
      MY_DYN_DATA.FREE (DATA_ACCESS_ARRAY(J));
    end loop;
  end ONE_CYCLE;

begin
  begin
    if ARGUMENT.GET_NBRE_ARG = 0 then
      NB_LOOP := 0;
    elsif ARGUMENT.GET_NBRE_ARG = 1 then
      NB_LOOP := NATURAL'VALUE(ARGUMENT.GET_PARAMETER);
    else
      raise CONSTRAINT_ERROR;
    end if;
  exception
    when others =>
      TEXT_IO.PUT_LINE ("Wrong argument. Usage : "
      & ARGUMENT.GET_PROGRAM_NAME & " [ <nb_iteration> ]");
      raise ABORT_EXCEPTION;
  end;

  begin
    ONE_CYCLE;
  exception
    when STORAGE_ERROR =>
      TEXT_IO.PUT_LINE ("The test cannot be performed: "
          & "Even one iteration raises STORAGE_ERROR.");
      TEXT_IO.PUT_LINE ("Lower MAX_DATA in source file, recompile and retry.");
      raise ABORT_EXCEPTION;
  end;

  TEXT_IO.PUT_LINE ("This test succeeds if no STORAGE_ERROR is raised.");
  TEXT_IO.PUT ("Performinig ");
  if NB_LOOP = 0 then
    TEXT_IO.PUT (" infinite");
  else
    TEXT_IO.PUT (NATURAL'IMAGE(NB_LOOP));
  end if;
  TEXT_IO.PUT_LINE (" iterations, each of them consisting in");
  TEXT_IO.PUT_LINE (" allocating " & INTEGER'IMAGE(MAX_DATA) & " objects of "
                    & INTEGER'IMAGE(DATA'LAST) & " bytes then freeing them.");


  if NB_LOOP = 0 then
    loop
      ONE_CYCLE;
    end loop;
  else
    for I in 1 .. NB_LOOP loop
      ONE_CYCLE;
    end loop;
  end if;

  TEXT_IO.PUT_LINE ("Test successful.");
exception
  when STORAGE_ERROR =>
    TEXT_IO.PUT_LINE ("Test has failed.");
  when ABORT_EXCEPTION =>
    null;
  when others =>
    TEXT_IO.PUT_LINE ("Unexpected exception was raised.");
    raise;
end T_DD;

