with TEXT_IO;

with NORMAL;
with ARGUMENT;
with MY_IO;

with TYPES;
with FILE;

procedure T_FILE is
  DIM : NATURAL;

begin
  if ARGUMENT.GET_NBRE_ARG /= 1 then
    TEXT_IO.PUT_LINE ("Syntax error. Usage : t_file <file_name>");
    return;
  end if;

  declare
    MATTRIX : TYPES.MATTRIX_REC := FILE.READ (ARGUMENT.GET_PARAMETER);
  begin


    TEXT_IO.PUT_LINE ("Kind is " & TYPES.MATTRIX_KIND_LIST'IMAGE(FILE.GET_KIND));
    DIM := MATTRIX.DIM;

    for I in 1 .. MATTRIX.DIM loop
      for J in 1 .. MATTRIX.DIM loop
        TEXT_IO.PUT (NORMAL(MATTRIX.NOTES(I,J), 5) & " ");
      end loop;
      TEXT_IO.NEW_LINE;
    end loop;
  end;

  
  for I in 1 .. DIM loop
    for J in 1 .. DIM loop
      MY_IO.PUT (FILE.GET_NOTE(I,J)); MY_IO.PUT(" ");
    end loop;
    TEXT_IO.NEW_LINE;
  end loop;

exception
  when FILE.READ_ERROR =>
    null;
end T_FILE;
