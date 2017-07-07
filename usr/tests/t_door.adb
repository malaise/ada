with Protected_Put, Doors;
procedure T_Door is
  pragma Priority(10);

  Door : Doors.Door;

  Nb_Clients : constant := 4;
  Nb_Loops : constant := 3;
  Nb_Waiters : constant := 3;
  Stopping : Boolean := False;
  function Stop return Boolean is (Stopping);
  procedure Stop is
  begin
    Stopping := True;
  end Stop;

  task type Client is
    entry Init (No : in Positive);
    entry Resume;
  end Client;

  task body Client is
    Me : Positive;
    Res : Boolean;
  begin
    accept Init (No : in Positive) do
      Me := No;
      Protected_Put.Put_Line_Output ("Client " & Me'Img & " initialised");
    end Init;
    for I in 1 .. Nb_Loops loop
      Protected_Put.Put_Line_Output ("Client " & Me'Img
                                   & " getting access to door");
      Door.Get;

      if Me = 1 then
        -- First task make conditional waiting
        Protected_Put.Put_Line_Output ("Client " & Me'Img
                                     & " trying to wait on door");
        if I = 1 then
          -- This one will fail
          Res := Door.Wait (0.1);
        else
          -- These ones will succeed
          Res := Door.Wait (10.0);
        end if;
      else
        Protected_Put.Put_Line_Output ("Client " & Me'Img & " waiting on door");
        Door.Wait;
        Res := True;
      end if;
      if Res then
        Protected_Put.Put_Line_Output ("Client " & Me'Img
                                     & " released on door");
        Door.Release;
      else
        Protected_Put.Put_Line_Output ("Client " & Me'Img & " giving up");
      end if;
      delay 1.0;
    end loop;

    Protected_Put.Put_Line_Output ("Client " & Me'Img & " suspended");
    accept Resume;
    Protected_Put.Put_Line_Output ("Client " & Me'Img & " resumed");

    -- Now loop waiting on the door until stopped
    loop
      Protected_Put.Put_Line_Output ("Client " & Me'Img & " waiting");
      Door.Get;
      Door.Wait;
      Door.Release;
      Protected_Put.Put_Line_Output ("Client " & Me'Img & " released");
      delay 0.5;
      exit when Stop;
    end loop;
    Protected_Put.Put_Line_Output ("Client " & Me'Img & " terminating");
  end Client;

  Clients : array (1 .. Nb_Clients) of Client;

begin -- T_Cond
  -- Set door capacity
  Door.Get;
  Door.Set_Nb_Waiters (Nb_Waiters);
  Door.Release;

  -- Init the clients
  Protected_Put.Put_Line_Output ("Main initializing clients");
  for I in 1 .. Nb_Clients loop
    Clients(I).Init (I);
    delay 0.1;
  end loop;
  delay 0.5;
  Protected_Put.Put_Line_Output ("Main waiting some time");
  delay 5.0;

  -- Release all clients
  Protected_Put.Put_Line_Output ("Main opening door");
  Door.Get;
  Door.Set_Nb_Waiters (Doors.Open);
  Door.Release;

  Protected_Put.Put_Line_Output ("Main resuming");
  for I in 1 .. Nb_Clients loop
    Clients(I).Resume;
  end loop;

  -- Lock the door
  Protected_Put.Put_Line_Output ("Main locking door");
  Door.Get;
  Door.Set_Nb_Waiters (Doors.Closed);
  Door.Release;
  delay 1.0;

  -- Check that we can bypass
  Protected_Put.Put_Line_Output ("Main bypassing door");
  Door.Get;
  Door.Wait (Doors.Pass);
  Door.Release;
  delay 1.0;

  -- Unlock the door
  Protected_Put.Put_Line_Output ("Main unlocking door");
  Door.Get;
  Door.Set_Nb_Waiters (Doors.Open);
  Door.Release;
  delay 5.0;

  -- Terminate
  Protected_Put.Put_Line_Output ("Main stopping");
  Stop;
  Protected_Put.Put_Line_Output ("Main terminating");
end T_Door;

