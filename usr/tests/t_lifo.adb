with Queues;
with Basic_Proc;
procedure T_Lifo is

  Size : constant := 5;
  N : Positive;

  package My_Lifo is new Queues.Lifo (
   Size => Size,
   Item => Positive);
  Lifo : My_Lifo.Lifo_Type;

  procedure Dump is
    V : Positive;
  begin
    for I in 1 .. Size loop
      My_Lifo.Look_Last (Lifo, V, I);
      Basic_Proc.Put_Line_Output ("Look "  & Integer'Image (I) &
                       " --> " & Positive'Image (V) );
    end loop;
  exception
    when My_Lifo.Lifo_Not =>
      Basic_Proc.Put_Line_Output ("Look done");
    when My_Lifo.Lifo_Empty =>
      Basic_Proc.Put_Line_Output ("LIFO empty");
      raise;
  end Dump;

  procedure Push (I : in Positive) is
  begin
    My_Lifo.Push (Lifo, I);
  exception
    when My_Lifo.Lifo_Full =>
      Basic_Proc.Put_Line_Output ("LIFO full. Discard last then push again");
      My_Lifo.Discard_Last (Lifo);
      My_Lifo.Push (Lifo, I);
  end Push;


begin
  Basic_Proc.Put_Line_Output ("Lifo size is " & Positive'Image (Size));

  for I in 1 .. 10 loop
    Basic_Proc.Put_Line_Output ("Push " & Positive'Image (I));
    Push (I);
    Dump;
  end loop;

  loop
    Basic_Proc.New_Line_Output;
    My_Lifo.Pop (Lifo, N);
    Basic_Proc.Put_Line_Output ("Pop ==> " & Positive'Image(N));
    begin
      Dump;
    exception
      when My_Lifo.Lifo_Empty => exit;
    end;
  end loop;

end T_Lifo;

