with Basic_Proc, Door_Manager;

procedure T_Door is
  pragma Priority(10);

  Door : Door_Manager.Door;

  Nb_Clients : constant := 4;
  Nb_Loops : constant := 3;
  Nb_Waiters : constant := 3;

  task type Client is
    entry Init (No : in Positive);
  end Client;

  task body Client is
    Me : Positive;
    Res : Boolean;
  begin
    accept Init (No : in Positive) do
      Me := No;
      Basic_Proc.Put_Line_Output ("Client " & Me'Img & " initialised");
    end Init;
    for I in 1 .. Nb_Loops loop
      Basic_Proc.Put_Line_Output ("Client " & Me'Img
                          & " getting access to door");
      Door_Manager.Get (Door);

      if Me = 1 then
        -- First task make conditional waiting
        Basic_Proc.Put_Line_Output ("Client " & Me'Img
                            & " trying to wait on door");
        if I = 1 then
          -- This one will fail
          Res := Door_Manager.Wait (Door, 0.1);
        else
          -- These ones will succeed
          Res := Door_Manager.Wait (Door, 10.0);
        end if;
      else
        Basic_Proc.Put_Line_Output ("Client " & Me'Img
                            & " waiting on door");
        Door_Manager.Wait (Door);
        Res := True;
      end if;
      if Res then
        Basic_Proc.Put_Line_Output ("Client " & Me'Img
                            & " released on door");
        Door_Manager.Release (Door);
      else
        Basic_Proc.Put_Line_Output ("Client " & Me'Img
                           & " giving up");
      end if;
      delay 1.0;
    end loop;
    Basic_Proc.Put_Line_Output ("Client " & Me'Img & " terminating");
  end Client;

  Clients : array (1 .. Nb_Clients) of Client;

begin -- T_Cond
  -- Set door capacity
  Door_Manager.Get (Door);
  Door_Manager.Set_Nb_Waiters (Door, Nb_Waiters);
  Door_Manager.Release (Door);

  -- Init the clients
  Basic_Proc.Put_Line_Output ("Main initializing clients");
  for I in 1 .. Nb_Clients loop
    Clients(I).Init (I);
    delay 0.1;
  end loop;
  delay 0.5;
  Basic_Proc.New_Line_Output;
  delay 10.0;

  -- Release all clients
  Door_Manager.Get (Door);
  Door_Manager.Set_Nb_Waiters (Door, 1);
  Door_Manager.Release (Door);

  -- Terminate
  Basic_Proc.Put_Line_Output ("Main terminating");
  Basic_Proc.New_Line_Output;
end T_Door;

