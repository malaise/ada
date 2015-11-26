with Protected_Put, Locks;
procedure T_Lock is
  pragma Priority(10);

  Lock : Locks.Lock;

  Nb_Clients : constant := 4;

  task type Client is
    entry Init (No : in Positive);
  end Client;

  task body Client is
    Me : Positive;
    First : Boolean;
    Res : Boolean;
  begin
    accept Init (No : in Positive) do
      Me := No;
      Protected_Put.Put_Line_Output ("Client " & Me'Img & " initialised");
    end Init;

    First := True;
    loop
      if Me = 1 then
        -- First task make conditional waiting
        Protected_Put.Put_Line_Output ("Client " & Me'Img
                                     & " trying to wait on lock");
        if First then
          -- This one will fail
          Res := Lock.Wait (0.1);
          First := False;
        else
          -- These ones will succeed
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

begin -- T_Cond

  -- Close the lock
  Lock.Close;

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

