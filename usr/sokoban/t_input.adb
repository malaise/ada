with CON_IO;
with SOK_INPUT;

procedure T_INPUT is
  KEY : SOK_INPUT.KEY_LIST;
begin

  CON_IO.INIT;
  CON_IO.RESET_TERM;
  loop
    KEY := SOK_INPUT.GET_KEY;
    CON_IO.PUT (" " & SOK_INPUT.KEY_LIST'IMAGE(KEY) & "    " & ASCII.CR);
  end loop;
exception
  when SOK_INPUT.BREAK_REQUESTED =>
    CON_IO.PUT ("Break" & "      " & ASCII.CR);
    SOK_INPUT.END_OF_PROGRAM;
end T_INPUT;
