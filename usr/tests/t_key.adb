with CON_IO;
with MY_IO;
procedure T_KEY is

  KEY : NATURAL;
  CHAR : BOOLEAN;
  CTRL, SHIFT : BOOLEAN;

begin

  CON_IO.INIT;
  CON_IO.RESET_TERM;
  KEY := 0;
  CHAR := TRUE;
  CTRL := FALSE;
  SHIFT := FALSE;
  loop
    if KEY = 0 and then CHAR and then not CTRL and then NOT SHIFT then
      -- refresh
      CON_IO.MOVE;
      CON_IO.PUT_LINE ("Exit with CTRL_BREAK");
    end if;
    CON_IO.GET_KEY (KEY, CHAR, CTRL, SHIFT);
    MY_IO.PUT (KEY, BASE => 16, WIDTH => 6);

    if CHAR then
      MY_IO.PUT (" CHAR ");
      if KEY = 8 then
        MY_IO.PUT ("Backspace");
      elsif KEY in
       CHARACTER'POS(CHARACTER'FIRST) .. CHARACTER'POS(CHARACTER'LAST) then
        MY_IO.PUT ('>' & CHARACTER'VAL(KEY) & '<');
      else
        MY_IO.PUT (" non ADA character");
      end if;
    end if;
    MY_IO.PUT (" " & BOOLEAN'IMAGE(CTRL) & " " & BOOLEAN'IMAGE(SHIFT));
    MY_IO.NEW_LINE;
    exit when KEY = 0 and then CTRL and then not SHIFT;
  end loop;
end T_KEY;
