
-- One argument : file name
-- Tests get_line

with TEXT_IO;
with TEXT_HANDLER, ARGUMENT, GET_LINE, NORMAL;

procedure T_GET_LINE is

  MAX_LINE_LEN : constant := 1024;
  MAX_WORD_NB  : constant := 500;
  MAX_WORD_LEN : constant := 30;

begin
  -- Check syntax
  if ARGUMENT.GET_NBRE_ARG /= 1 then
    TEXT_IO.PUT_LINE ("ERROR. Syntax : t_get_line <file_name>");
    return;
  end if;

  declare
    package MY_GET_LINE is new GET_LINE (
      MAX_WORD_LEN => MAX_WORD_LEN,
      MAX_WORD_NB  => MAX_WORD_NB,
      MAX_LINE_LEN => MAX_LINE_LEN);

    LINE  : MY_GET_LINE.LINE_ARRAY;

  begin

    -- open file
    begin
      MY_GET_LINE.OPEN (ARGUMENT.GET_PARAMETER);
    exception
      when others =>
        TEXT_IO.PUT_LINE ("ERROR opening file " & ARGUMENT.GET_PARAMETER & ".");
        raise;
    end;


    loop
      MY_GET_LINE.GET_WORDS (LINE);
      TEXT_IO.PUT (NORMAL (INTEGER (MY_GET_LINE.GET_LINE_NO), 3, GAP => '0') & " -> ");
      TEXT_IO.PUT (NORMAL (MY_GET_LINE.GET_WORD_NUMBER, 3) & ":");
      for I in 1 .. MY_GET_LINE.GET_WORD_NUMBER loop
        TEXT_IO.PUT (">" & TEXT_HANDLER.VALUE (LINE(I)) & "<");
      end loop;
      TEXT_IO.NEW_LINE;
      MY_GET_LINE.READ_NEXT_LINE;
    end loop;

  exception
    when MY_GET_LINE.NO_MORE_LINE =>
      TEXT_IO.PUT_LINE ("Done.");
  end;

end T_GET_LINE;
