with Queues;
with My_Io;
procedure T_Lifo is

  Size : constant := 5;
  N : Positive;

  package My_Lifo is new Queues.Lifo (
   Size => Size,
   Item => Positive);

  procedure Dump is
    V : Positive;
  begin
    for I in 1 .. Size loop
      My_Lifo.Look_Last (V, I);
      My_Io.Put_Line ("Look "  & Integer'Image (I) &
                       " --> " & Positive'Image (V) );
    end loop;
  exception
    when My_Lifo.Lifo_Not =>
      My_Io.Put_Line ("Look done");
    when My_Lifo.Lifo_Empty =>
      My_Io.Put_Line ("LIFO empty");
      raise;
  end Dump;

  procedure Push (I : in Positive) is
  begin
    My_Lifo.Push (I);
  exception
    when My_Lifo.Lifo_Full =>
      My_Io.Put_Line ("LIFO full. Discard last then push again");
      My_Lifo.Discard_Last;
      My_Lifo.Push (I);
  end Push;


begin
  My_Io.Put_Line ("Lifo size is " & Positive'Image (Size));

  for I in 1 .. 10 loop
    My_Io.Put_Line ("Push " & Positive'Image (I));
    Push (I);
    Dump;
  end loop;

  loop
    My_Io.New_Line;
    My_Lifo.Pop (N);
    My_Io.Put_Line ("Pop ==> " & Positive'Image(N));
    begin
      Dump;
    exception
      when My_Lifo.Lifo_Empty => exit;
    end;
  end loop;

end T_Lifo;


