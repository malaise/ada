with TEXT_IO;
with TEXT_HANDLER;
with SELECT_FILE;

procedure T_SELECT_FILE is

  READ : BOOLEAN;
  FILE : TEXT_HANDLER.TEXT(500);

  procedure INIT is
  begin
    NULL;
  end INIT;

  function MY_SELECT_FILE is new SELECT_FILE(INIT);

begin
  READ := TRUE;

  loop
    TEXT_HANDLER.SET (FILE, MY_SELECT_FILE (10, TEXT_HANDLER.VALUE(FILE), READ));
    TEXT_IO.PUT_LINE (TEXT_HANDLER.VALUE(FILE));
    exit when TEXT_HANDLER.EMPTY(FILE);
    READ := not READ;
  end loop;
end T_SELECT_FILE;

