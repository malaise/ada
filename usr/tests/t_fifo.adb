with Queues;
with Basic_Proc;
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
      Basic_Proc.Put_Line_Output ("Look "  & Integer'Image (I) &
                       " --> " & Positive'Image (V) );
    end loop;
  exception
    when My_Fifo.Fifo_Not =>
      Basic_Proc.Put_Line_Output ("Look done");
    when My_Fifo.Fifo_Empty =>
      Basic_Proc.Put_Line_Output ("FIFO empty");
      raise;
  end Dump;

  procedure Push (I : in Positive) is
  begin
    My_Fifo.Push (Fifo, I);
  exception
    when My_Fifo.Fifo_Full =>
      Basic_Proc.Put_Line_Output ("FIFO full. Discard last then push again");
      My_Fifo.Discard_Last (Fifo);
      My_Fifo.Push (Fifo, I);
  end Push;


begin
  Basic_Proc.Put_Line_Output ("Fifo size is " & Positive'Image (Size));

  for I in 1 .. 10 loop
    Basic_Proc.Put_Line_Output ("Push " & Positive'Image (I));
    Push (I);
    Dump;
  end loop;

  loop
    Basic_Proc.New_Line_Output;
    My_Fifo.Pop (Fifo, N);
    Basic_Proc.Put_Line_Output ("Pop ==> " & Positive'Image(N));
    begin
      Dump;
    exception
      when My_Fifo.Fifo_Empty => exit;
    end;
  end loop;

end T_Fifo;

