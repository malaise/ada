with Ada.Text_Io;
with My_Io, Door_Manager, Schedule, Argument, Basic_Proc, Upper_Char;

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
      Ada.Text_Io.Put_Line ("Client " & Me'Img & " initialised");
    end Init;
    for I in 1 .. Nb_Loops loop
      Ada.Text_Io.Put_Line ("Client " & Me'Img
                          & " getting access to door");
      Door_Manager.Get (Door);

      if Me = 1 then
        -- First task make conditional waiting
        Ada.Text_Io.Put_Line ("Client " & Me'Img
                            & " trying to wait on door");
        if I = 1 then
          -- This one will fail
          Res := Door_Manager.Wait (Door, 0.1);
        else
          -- These ones will succeed
          Res := Door_Manager.Wait (Door, 10.0);
        end if;
      else
        Ada.Text_Io.Put_Line ("Client " & Me'Img
                            & " waiting on door");
        Door_Manager.Wait (Door);
        Res := True;
      end if;
      if Res then
        Ada.Text_Io.Put_Line ("Client " & Me'Img
                            & " released on door");
        Door_Manager.Release (Door);
      else
        Ada.Text_Io.Put_Line ("Client " & Me'Img
                           & " giving up");
      end if;
      delay 1.0;
    end loop;
    Ada.Text_Io.Put_Line ("Client " & Me'Img & " terminating");
  end Client;

  Clients : array (1 .. Nb_Clients) of Client;

begin -- T_Cond
  -- Set door capacity
  Door_Manager.Get (Door);
  Door_Manager.Set_Nb_Waiters (Door, Nb_Waiters);
  Door_Manager.Release (Door);

  -- Init the clients
  Ada.Text_Io.Put_Line ("Main initializing clients");
  for I in 1 .. Nb_Clients loop
    Clients(I).Init (I);
    delay 0.1;
  end loop;
  delay 0.5;
  Ada.Text_Io.New_Line;
  delay 10.0;

  -- Release all clients
  Door_Manager.Get (Door);
  Door_Manager.Set_Nb_Waiters (Door, 1);
  Door_Manager.Release (Door);

  -- Terminate
  Ada.Text_Io.Put_Line ("Main terminating");
  Ada.Text_Io.New_Line;
end T_Door;

