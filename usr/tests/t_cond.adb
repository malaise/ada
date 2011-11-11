with Basic_Proc, Condition_Manager;

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
      Basic_Proc.Put_Line_Output ("Client " & Me'Img & " initialised");
    end Init;
    for I in 1 .. Nb_Loops loop
      Basic_Proc.Put_Line_Output ("Client " & Me'Img
                          & " getting access");
      Condition_Manager.Get (Cond);

      if Me = 1 then
        -- First task makes conditional waiting
        Basic_Proc.Put_Line_Output ("Client " & Me'Img
                            & " trying to wait on condition");
        if I = 1 then
          -- This one will fail
          Res := Condition_Manager.Wait (Cond, 0.1);
        else
          -- These ones will succeed
          Res := Condition_Manager.Wait (Cond, 10.0);
        end if;
      else
        Basic_Proc.Put_Line_Output ("Client " & Me'Img
                            & " waiting on condition");
        Condition_Manager.Wait (Cond);
        Res := True;
      end if;
      if Res then
        Basic_Proc.Put_Line_Output ("Client " & Me'Img
                            & " released on condition, releasing access");
        Condition_Manager.Release (Cond);
      else
        Basic_Proc.Put_Line_Output ("Client " & Me'Img
                           & " giving up");
      end if;

    end loop;
    Basic_Proc.Put_Line_Output ("Client " & Me'Img & " terminating");
  end Client;

  Clients : array (1 .. Nb_Clients) of Client;

begin -- T_Cond
  -- Init the clients
  Basic_Proc.Put_Line_Output ("Main initializing clients");
  for I in 1 .. Nb_Clients loop
    Clients(I).Init (I);
  end loop;
  delay 0.5;
  Basic_Proc.New_Line_Output;
  delay 1.0;

  -- Signal each client plus some of them twice
  for I in 1 .. Nb_Clients + 2 loop
    Basic_Proc.Put_Line_Output ("Main signaling on condition");
    Condition_Manager.Signal (Cond);
    delay 0.1;
    Basic_Proc.New_Line_Output;
    delay 1.0;
  end loop;

  -- Broadcast enough times so each client reaches end
  for I in 1 ..Nb_Loops loop
    Basic_Proc.Put_Line_Output ("Main broadcasting on condition");
    Condition_Manager.Broadcast (Cond);
    delay 0.1;
    Basic_Proc.New_Line_Output;
    delay 1.0;
  end loop;

  Basic_Proc.Put_Line_Output ("Main signaling on condition");
  Condition_Manager.Signal (Cond);
  delay 0.1;
  Basic_Proc.New_Line_Output;
  delay 1.0;

  -- Terminate
  Basic_Proc.Put_Line_Output ("Main terminating");
  Basic_Proc.New_Line_Output;
end T_Cond;

