with TEXT_IO; -- For exception trace
with MY_IO;
with GRID_2;

procedure TG2 is
  KEY : constant STRING := "C'est la cle 2";
  STR1 : constant GRID_2.LONG_STRING := "Ah que coucou!";
  STR2 : GRID_2.LONG_STRING (1 .. STR1'LENGTH);
  STR3 : GRID_2.LONG_STRING (1 .. STR1'LENGTH);
begin
  STR2 := GRID_2.ENCODE (KEY, STR1);
  MY_IO.PUT (">");
  for I in STR2'RANGE loop
    MY_IO.PUT(STR2(I));
  end loop;
  MY_IO.PUT_LINE("<");
  STR3 := GRID_2.DECODE (KEY, STR2);
  MY_IO.PUT (">");
  for I in STR3'RANGE loop
    MY_IO.PUT(STR3(I));
  end loop;
  MY_IO.PUT_LINE("<");
end TG2;

