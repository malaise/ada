with MY_IO, NORMAL;
procedure T_NORMAL is
  I : INTEGER;
  LEN : POSITIVE;
  RIGHT : BOOLEAN;
  GAP : CHARACTER;
begin

  loop
    loop
      begin
        MY_IO.PUT ("I ? : "); MY_IO.GET (I);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        MY_IO.PUT ("LEN ? : "); MY_IO.GET (LEN);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        MY_IO.PUT ("RIGHT ? : "); MY_IO.GET (RIGHT);
        exit;
      exception
        when others => null;
      end;
    end loop;
    loop
      begin
        MY_IO.PUT ("GAP ? : "); MY_IO.GET (GAP);
        exit;
      exception
        when others => null;
      end;
    end loop;

    MY_IO.PUT_LINE ("0         1          2         3         4         5");
    MY_IO.PUT_LINE ("0123456789012345678980123456789012345678901234567890");
    MY_IO.PUT_LINE ('>' & NORMAL (I, LEN, RIGHT, GAP) & '<');
    MY_IO.NEW_LINE;
  end loop;
end T_NORMAL;
