with NORMAL;
with MY_IO; use MY_IO;
procedure T_NORM is
  I : INTEGER;
  M : POSITIVE;
  R : BOOLEAN;
begin
  loop
    begin
      PUT ("I ? "); GET (I);
      PUT ("Max ? "); GET (M);
      PUT ("Right (TRUE, FALSE) ? "); GET (R);
      PUT_LINE (" 12345678901234567890");
      PUT_LINE (">" & NORMAL (I, M, R) & "<");
    exception
      when others =>
        PUT_LINE ("Exception.");
    end;
  end loop;
end T_NORM;

