with QUEUES;
with MY_IO;
procedure T_LIFO is

  SIZE : constant := 5;
  N : POSITIVE;

  package MY_LIFO is new QUEUES.LIFO (
   SIZE => SIZE,
   ITEM => POSITIVE);

  procedure DUMP is
    V : POSITIVE;
  begin
    for I in 1 .. SIZE loop
      MY_LIFO.LOOK_LAST (V, I);
      MY_IO.PUT_LINE ("Look "  & INTEGER'IMAGE (I) &
                       " --> " & POSITIVE'IMAGE (V) );
    end loop;
  exception
    when MY_LIFO.LIFO_NOT =>
      MY_IO.PUT_LINE ("Look done");
    when MY_LIFO.LIFO_EMPTY =>
      MY_IO.PUT_LINE ("LIFO empty");
      raise;
  end DUMP;

  procedure PUSH (I : in POSITIVE) is
  begin
    MY_LIFO.PUSH (I);
  exception
    when MY_LIFO.LIFO_FULL =>
      MY_IO.PUT_LINE ("LIFO full. Discard last then push again");
      MY_LIFO.DISCARD_LAST;
      MY_LIFO.PUSH (I);
  end PUSH;


begin
  MY_IO.PUT_LINE ("Lifo size is " & POSITIVE'IMAGE (SIZE));

  for I in 1 .. 10 loop
    MY_IO.PUT_LINE ("Push " & POSITIVE'IMAGE (I));
    PUSH (I);
    DUMP;
  end loop;

  loop
    MY_IO.NEW_LINE;
    MY_LIFO.POP (N);
    MY_IO.PUT_LINE ("Pop ==> " & POSITIVE'IMAGE(N));
    begin
      DUMP;
    exception
      when MY_LIFO.LIFO_EMPTY => exit;
    end;
  end loop;

end T_LIFO;


