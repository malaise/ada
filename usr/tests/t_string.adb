with MY_IO, STRING_MNG;
procedure T_STRING is

  STR : STRING(1 .. 500);
  STR_LEN : NATURAL;
  LEN : POSITIVE;
  RIGHT : BOOLEAN;
  GAP : CHARACTER;
  SHOW_TRUNC : BOOLEAN;

begin

  loop
    MY_IO.PUT ("STR ? "); MY_IO.GET_LINE (STR, STR_LEN);
    MY_IO.PUT ("LEN ? "); MY_IO.GET(LEN);
    MY_IO.PUT ("RIGHT ? "); MY_IO.GET(RIGHT);
    MY_IO.PUT ("GAP ? "); MY_IO.GET(GAP);
    MY_IO.PUT ("SHOW_TRUNC ? "); MY_IO.GET(SHOW_TRUNC);
    MY_IO.SKIP_LINE;

    MY_IO.PUT_LINE ("String: |" & STR(1 .. STR_LEN) & "|   len: "
                  & INTEGER'IMAGE(STR_LEN));
    MY_IO.PUT_LINE (
        "First: " 
      & NATURAL'IMAGE(STRING_MNG.PARSE_SPACES(STR(1 .. STR_LEN), TRUE))
      & " Last: "
      & NATURAL'IMAGE(STRING_MNG.PARSE_SPACES(STR(1 .. STR_LEN), FALSE)));

    MY_IO.PUT_LINE (
        "Procuste: |" 
      & STRING_MNG.PROCUSTE(STR(1 .. STR_LEN), LEN, RIGHT, GAP, SHOW_TRUNC)
      & "|");
    MY_IO.NEW_LINE;
  end loop;
end T_STRING;

