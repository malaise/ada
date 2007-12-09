with Queues;
with My_Io;
procedure T_Fifo is

  Size : constant := 5;
  N : Positive;

  package My_Fifo is new Queues.Fifo (
   Size => Size,
   Item => Positive);
  Fifo : My_Fifo.Fifo_Type;

  procedure Dump is
    V : Positive;
  begin
    for I in 1 .. Size loop
      My_Fifo.Look_Last (Fifo, V, I);
      My_Io.Put_Line ("Look "  & Integer'Image (I) &
                       " --> " & Positive'Image (V) );
    end loop;
  exception
    when My_Fifo.Fifo_Not =>
      My_Io.Put_Line ("Look done");
    when My_Fifo.Fifo_Empty =>
      My_Io.Put_Line ("FIFO empty");
      raise;
  end Dump;

  procedure Push (I : in Positive) is
  begin
    My_Fifo.Push (Fifo, I);
  exception
    when My_Fifo.Fifo_Full =>
      My_Io.Put_Line ("FIFO full. Discard last then push again");
      My_Fifo.Discard_Last (Fifo);
      My_Fifo.Push (Fifo, I);
  end Push;


begin
  My_Io.Put_Line ("Fifo size is " & Positive'Image (Size));

  for I in 1 .. 10 loop
    My_Io.Put_Line ("Push " & Positive'Image (I));
    Push (I);
    Dump;
  end loop;

  loop
    My_Io.New_Line;
    My_Fifo.Pop (Fifo, N);
    My_Io.Put_Line ("Pop ==> " & Positive'Image(N));
    begin
      Dump;
    exception
      when My_Fifo.Fifo_Empty => exit;
    end;
  end loop;

end T_Fifo;

