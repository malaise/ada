with QUEUES;
with MY_IO;
procedure T_FIFO is

  SIZE : constant := 5;
  N : POSITIVE;

  package MY_FIFO is new QUEUES.FIFO (
   SIZE => SIZE,
   ITEM => POSITIVE);

  procedure DUMP is
    V : POSITIVE;
  begin
    for I in 1 .. SIZE loop
      MY_FIFO.LOOK_LAST (V, I);
      MY_IO.PUT_LINE ("Look "  & INTEGER'IMAGE (I) &
                       " --> " & POSITIVE'IMAGE (V) );
    end loop;
  exception
    when MY_FIFO.FIFO_NOT =>
      MY_IO.PUT_LINE ("Look done");
    when MY_FIFO.FIFO_EMPTY =>
      MY_IO.PUT_LINE ("FIFO empty");
      raise;
  end DUMP;

  procedure PUSH (I : in POSITIVE) is
  begin
    MY_FIFO.PUSH (I);
  exception
    when MY_FIFO.FIFO_FULL =>
      MY_IO.PUT_LINE ("FIFO full. Discard last then push again");
      MY_FIFO.DISCARD_LAST;
      MY_FIFO.PUSH (I);
  end PUSH;


begin
  MY_IO.PUT_LINE ("Fifo size is " & POSITIVE'IMAGE (SIZE));

  for I in 1 .. 10 loop
    MY_IO.PUT_LINE ("Push " & POSITIVE'IMAGE (I));
    PUSH (I);
    DUMP;
  end loop;

  loop
    MY_IO.NEW_LINE;
    MY_FIFO.POP (N);
    MY_IO.PUT_LINE ("Pop ==> " & POSITIVE'IMAGE(N));
    begin
      DUMP;
    exception
      when MY_FIFO.FIFO_EMPTY => exit;
    end;
  end loop;

end T_FIFO;


