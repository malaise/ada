with Protected_Put, Mutexes, Mixed_Str;
procedure T_Recursive_Mutex is

  M : Mutexes.Mutex (Mutexes.Simple, Recursive => True);
  B : Boolean;

  task T is
    entry Start;
    entry Stop;
  end T;


  task body T is
  begin
    accept Start;
    loop
      Protected_Put.Put_Line_Output ("   Task gets mutex");
      M.Get;
      Protected_Put.Put_Line_Output ("   Task has got mutex");
      delay 3.0;
      Protected_Put.Put_Line_Output ("   Task releases mutex");
      M.Release;
      select
        accept Stop;
        exit;
      or
        delay 3.0;
      end select;
    end loop;
  end T;

begin
  -- Get mutex twice
  Protected_Put.Put_Line_Output ("Getting mutex");
  M.Get;
  Protected_Put.Put_Line_Output ("Getting mutex");
  M.Get;

  -- Start task and get mutex a third time
  Protected_Put.Put_Line_Output ("Starting task");
  T.Start;
  Protected_Put.Put_Line_Output ("Getting mutex");
  M.Get;
  Protected_Put.New_Line_Output;

  -- Release fully mutex (task takes it)
  Protected_Put.Put_Line_Output ("Releasing fully mutex");
  M.Release(Fully => True);

  -- Get mutex => Ko, Ko... Ok
  loop
    Protected_Put.Put_Line_Output ("Trying to get mutex");
    B := M.Get (1.0);
    Protected_Put.Put_Line_Output ("Result is " & Mixed_Str (B'Img));
    exit when B;
  end loop;
  Protected_Put.New_Line_Output;

  -- Get mutex a second time then release twice
  Protected_Put.Put_Line_Output ("Getting mutex and waiting 2s");
  M.Get;
  delay 2.0;
  Protected_Put.Put_Line_Output ("Releasing mutex twice");
  M.Release;
  M.Release;

  -- The end
  Protected_Put.Put_Line_Output ("Stopping task");
  T.Stop;
  Protected_Put.Put_Line_Output ("Done");

end T_Recursive_Mutex;

