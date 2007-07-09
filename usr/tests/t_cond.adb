with Ada.Text_Io;
with My_Io, Condition_Manager, Schedule, Argument, Basic_Proc, Upper_Char;

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
      Ada.Text_Io.Put_Line ("Client " & Me'Img & " initialised");
    end Init;
    for I in 1 .. Nb_Loops loop
      Ada.Text_Io.Put_Line ("Client " & Me'Img
                          & " getting access");
      Condition_Manager.Get (Cond);

      if Me = 1 then
        -- First task makes conditional waiting
        Ada.Text_Io.Put_Line ("Client " & Me'Img
                            & " trying to wait on condition");
        if I = 1 then
          -- This one will fail
          Res := Condition_Manager.Wait (Cond, 0.1);
        else
          -- These ones will succeed
          Res := Condition_Manager.Wait (Cond, 10.0);
        end if;
      else
        Ada.Text_Io.Put_Line ("Client " & Me'Img
                            & " waiting on condition");
        Condition_Manager.Wait (Cond);
        Res := True;
      end if;
      if Res then
        Ada.Text_Io.Put_Line ("Client " & Me'Img
                            & " released on condition, releasing access");
        Condition_Manager.Release (Cond);
      else
        Ada.Text_Io.Put_Line ("Client " & Me'Img
                           & " giving up");
      end if;

    end loop;
    Ada.Text_Io.Put_Line ("Client " & Me'Img & " terminating");
  end Client;
     
  Clients : array (1 .. Nb_Clients) of Client;

begin -- T_Cond
  -- Init the clients
  Ada.Text_Io.Put_Line ("Main initializing clients");
  for I in 1 .. Nb_Clients loop
    Clients(I).Init (I);
  end loop;
  delay 0.5;
  Ada.Text_Io.New_Line;
  delay 1.0;

  -- Signal each client plus some of them twice
  for I in 1 .. Nb_Clients + 2 loop
    Ada.Text_Io.Put_Line ("Main signaling on condition");
    Condition_Manager.Signal (Cond);
    delay 0.1;
    Ada.Text_Io.New_Line;
    delay 1.0;
  end loop;

  -- Broadcast enough times so each client reaches end
  for I in 1 ..Nb_Loops loop
    Ada.Text_Io.Put_Line ("Main broadcasting on condition");
    Condition_Manager.Broadcast (Cond);
    delay 0.1;
    Ada.Text_Io.New_Line;
    delay 1.0;
  end loop;

  Ada.Text_Io.Put_Line ("Main signaling on condition");
  Condition_Manager.Signal (Cond);
  delay 0.1;
  Ada.Text_Io.New_Line;
  delay 1.0;

  -- Terminate
  Ada.Text_Io.Put_Line ("Main terminating");
  Ada.Text_Io.New_Line;
end T_Cond;

