with DOS;
with UPPER_STR;
with NORMAL;
with HASH;
with MY_IO;
with TEXT_HANDLER;
procedure T_HASH is

  package MY_HASH is new HASH.HASH_MNG (512, INTEGER);

  subtype TXT_P is TEXT_HANDLER.TEXT(500);
  TXT : TXT_P;
  INPUT : STRING (1 .. TXT.MAX_LEN);
  LEN : NATURAL;
  I : POSITIVE := 1;

  FOUND : MY_HASH.FOUND_REC;

  function STR (TXT : TXT_P) return STRING is
  begin
    return TEXT_HANDLER.VALUE(TXT) (3 .. TEXT_HANDLER.LENGTH(TXT));
  exception
    when others =>
      return "";
  end STR;

  function IMAGE (I : POSITIVE) return STRING is
  begin
    return NORMAL (I, 3, GAP => '0');
  end IMAGE;

begin

  loop
    MY_IO.NEW_LINE;

    MY_IO.PUT ("Store <>, Zreset <>, Find <>, Remove <>, Dump <>, EXIT ? ");
    MY_IO.GET_LINE (INPUT, LEN);
    TEXT_HANDLER.SET (TXT, INPUT(1 .. LEN));
    if TEXT_HANDLER.LENGTH(TXT) >= 3 and then TEXT_HANDLER.VALUE(TXT)(2) = ' ' then
      case TEXT_HANDLER.VALUE(TXT)(1) is
        when 'S' | 's' =>
          MY_HASH.STORE (STR(TXT), I);
          MY_IO.PUT_LINE (IMAGE(I) & " stored with key >" & STR(TXT) & "<.");
          I := I + 1;
        when 'Z' | 'z' =>
          MY_HASH.RESET_FIND (STR(TXT));
          MY_IO.PUT_LINE ("Search reset for key >" & STR(TXT) & "<.");
        when 'F' | 'f' =>
          FOUND := MY_HASH.FIND_NEXT (STR(TXT));
          if FOUND.FOUND then
            MY_IO.PUT_LINE ("Found " & IMAGE(FOUND.DATA) & " with key >" & STR(TXT) & "<.");
          else
            MY_IO.PUT_LINE ("No data found for key >" & STR(TXT) & "<.");
          end if;
        when 'R'| 'r' =>
          begin
            MY_HASH.REMOVE (STR(TXT));
            MY_IO.PUT_LINE ("Current data for key >" & STR(TXT) & "< removed.");
          exception
            when HASH.NOT_FOUND =>
              MY_IO.PUT_LINE ("Exception NOT_FOUND raised when removing data for key >"
                             & STR(TXT) & "<.");
          end;
        when 'D' | 'd' =>
          MY_IO.PUT_LINE ("Dumping data for key >" & STR(TXT) & "<:");
          MY_HASH.DUMP(STR(TXT));
        when others =>
          DOS.SOUND;
      end case;
    elsif UPPER_STR (TEXT_HANDLER.VALUE(TXT)) = "EXIT" then
      exit;
    else
      DOS.SOUND;
    end if;

  end loop;

end T_HASH;
