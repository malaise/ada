with Protected_Put, Condition_Manager;
procedure T_Cond is
  pragma Priority(10);

  Cond : Condition_Manager.Condition;

  Nb_Clients : constant := 4;
  Nb_Loops : constant := 3;

  task type Client is
    entry Init (No : in Positive);
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
                          & " getting access");
      Cond.Get;

      if Me = 1 then
        -- First task makes conditional waiting
        Protected_Put.Put_Line_Output ("Client " & Me'Img
                            & " trying to wait on condition");
        if I = 1 then
          -- This one will fail
          Res := Cond.Wait (0.1);
        else
          -- These ones will succeed
          Res := Cond.Wait (10.0);
        end if;
      else
        Protected_Put.Put_Line_Output ("Client " & Me'Img
                            & " waiting on condition");
        Cond.Wait;
        Res := True;
      end if;
      if Res then
        Protected_Put.Put_Line_Output ("Client " & Me'Img
                            & " released on condition, releasing access");
        Cond.Release;
      else
        Protected_Put.Put_Line_Output ("Client " & Me'Img
                           & " giving up");
      end if;

    end loop;
    Protected_Put.Put_Line_Output ("Client " & Me'Img & " terminating");
  end Client;

  Clients : array (1 .. Nb_Clients) of Client;

begin -- T_Cond
  -- Init the clients
  Protected_Put.Put_Line_Output ("Main initializing clients");
  for I in 1 .. Nb_Clients loop
    Clients(I).Init (I);
  end loop;
  delay 0.5;
  Protected_Put.Put_Line_Output ("Main waiting a bit");
  delay 1.0;

  -- Signal each client plus some of them twice
  for I in 1 .. Nb_Clients + 2 loop
    Protected_Put.Put_Line_Output ("Main signaling on condition");
    Cond.Signal;
    delay 0.1;
    Protected_Put.Put_Line_Output ("Main waiting a bit");
    delay 1.0;
  end loop;

  -- Broadcast enough times so each client reaches end
  for I in 1 ..Nb_Loops loop
    Protected_Put.Put_Line_Output ("Main broadcasting on condition");
    Cond.Broadcast;
    delay 0.1;
    Protected_Put.Put_Line_Output ("Main waiting a bit");
    Protected_Put.Put_Line_Output ("Main waiting a bit");
    delay 1.0;
  end loop;

  Protected_Put.Put_Line_Output ("Main signaling on condition");
  Cond.Signal;
  delay 0.1;
  Protected_Put.Put_Line_Output ("Main waiting a bit");
  delay 1.0;

  -- Terminate
  Protected_Put.Put_Line_Output ("Main terminating");
end T_Cond;

