-- Test a Lock with 4 clients
with Protected_Put, Locks;
procedure T_Lock is
  pragma Priority(10);

  Lock, Other_Lock : Locks.Lock;
  Key : Locks.Key_Type;

  Nb_Clients : constant := 4;

  task type Client is
    entry Init (No : in Positive);
  end Client;

  task body Client is
    Me : Positive;
    Count : Positive := 1;
    Res : Boolean;
  begin
    accept Init (No : in Positive) do
      Me := No;
      Protected_Put.Put_Line_Output ("Client " & Me'Img & " initialised");
    end Init;

    loop
      if Me = 1 then
        -- First task makes conditional waiting (2 failures on timeout then OK)
        if Count = 1 then
          -- This one will fail (Fake key)
          Protected_Put.Put_Line_Output ("Client " & Me'Img
                                     & " trying to wait on lock with fake");
          Res := Lock.Wait (0.1);
          Count := 2;
        elsif Count = 2 then
          -- This one will fail (Invalid key)
          Protected_Put.Put_Line_Output ("Client " & Me'Img
                              & " trying to wait on lock with invalid key");
          Res := Lock.Wait (0.1, Key);
          Count := 3;
        else
          -- These ones will succeed
          Protected_Put.Put_Line_Output ("Client " & Me'Img
                                     & " trying to wait on lock with pass");
          Res := Lock.Wait (10.0);
        end if;
      else
        Protected_Put.Put_Line_Output ("Client " & Me'Img & " waiting on lock");
        Lock.Wait;
        Res := True;
      end if;
      if Res then
        Protected_Put.Put_Line_Output ("Client " & Me'Img & " got lock");
        exit;
      else
        Protected_Put.Put_Line_Output ("Client " & Me'Img & " giving up");
      end if;
      delay 1.0;
    end loop;

  end Client;

  Clients : array (1 .. Nb_Clients) of Client;

begin -- T_Lock

  -- Close the lock
  Lock.Close;

  -- Get a key not valid for the lock
  Key := Other_Lock.Get_Key;

  -- Init the clients
  Protected_Put.Put_Line_Output ("Main initializing clients");
  for I in 1 .. Nb_Clients loop
    Clients(I).Init (I);
    delay 0.1;
  end loop;
  delay 0.5;
  Protected_Put.Put_Line_Output ("Main waiting some time");
  delay 5.0;

  -- Open the lock
  Lock.Open;

  -- Terminate
  Protected_Put.Put_Line_Output ("Main terminating");
end T_Lock;

