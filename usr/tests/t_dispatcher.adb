with Ada.Exceptions;
with My_Io, Rnd;
procedure T_Dispatcher is

  Max_Line_Number : constant := 3;

  subtype Line_Range is Natural range 0 .. Max_Line_Number;
  subtype Client_Range is Positive range 1 .. Max_Line_Number;
  No_Client_No : constant Line_Range := 0;


  function Get_Select (Id : Client_Range) return Line_Range is
    N : Integer;
  begin
    My_Io.Put ("Get " & Id'Img & " > ");
    My_Io.Get (N);
    if N not in Client_Range then
      N := No_Client_No;
    end if;
    return N;
  exception
    when Error:others =>
      My_Io.Put_Line ("Get_Select " & Id'Img & " EXCEPTION "
         & Ada.Exceptions.Exception_Name (Error));
      return 1;
  end Get_Select;

  protected Dispatcher is

    procedure Register (Id : out Line_Range);
    procedure Unregister (Id : in out Line_Range);

    entry Call_On (Id : in Client_Range);
    procedure Call_Off (Id : in Client_Range);

    procedure Wait (Id : in Client_Range);
    entry Get (Client_Range) (Kind : out Boolean);

  private
    Nb_Client : Line_Range := No_Client_No;
    Nb_Wait   : Line_Range := No_Client_No;
    Selected  : Line_Range := No_Client_No; 
    In_X      : Boolean := False;
  end Dispatcher;
  Tcheat : exception;

  protected body Dispatcher is

    procedure Register (Id : out Line_Range) is
    begin
      if Nb_Client = Client_Range'Last then
        Id := No_Client_No;
      else
        Nb_Client := Nb_Client + 1;
        Id := Nb_Client;
      end if;
    end Register;

    procedure Unregister (Id : in out Line_Range) is
    begin
      if Id /= No_Client_No and then Nb_Client /= 0 then
        Nb_Client := Nb_Client - 1;
        if Id = Selected then
          -- The running client is leaving: switch to another one
          Selected := Nb_Client;
          if Selected /= No_Client_No then
            -- Not the last one
            Nb_Wait := Nb_Wait - 1;
          end if;
        end if;
        Id := No_Client_No;
      end if;
    end Unregister;

    entry Call_On (Id : in Client_Range) when not In_X is
    begin
      if Id /= Selected then
       raise Tcheat;
      end if;
      In_X := True;
    end Call_On;

    procedure Call_Off (Id : in Client_Range) is
    begin
      if Id /= Selected then
       raise Tcheat;
      end if;
      In_X := False;
    end Call_Off;


    procedure Wait (Id : in Client_Range) is
    begin
      if Nb_Wait = Nb_Client - 1 then
        -- Last client ready to wait: call select which may set client
        Selected := Get_Select (Id);
        if Selected = No_Client_No then
          -- No specific client, get the same
          Selected := Id;
        end if;
      else
        Nb_Wait := Nb_Wait + 1;
      end if;
    end Wait;
    
    entry Get (for Id in Client_Range) (Kind : out Boolean)
              when Id = Selected is
    begin
      Kind := Rnd.Int_Random (0, 3) /= 0;
    end Get;
  end Dispatcher;

  task type Client is
    entry Stop;
  end Client;

  task body Client is
    Id : Line_Range;
    Kind : Boolean;

    procedure Call_X is
    begin
      Dispatcher.Call_On (Id);
      My_Io.Put_Line ("Client " & Id'Img & " in X");
      Dispatcher.Call_Off (Id);
    end Call_X;
  begin
    Dispatcher.Register (Id);
    if Id = No_Client_No then
      My_Io.Put_Line ("Client refused");
      select
        accept Stop;
      or
        terminate;
      end select;
    end if;
    My_Io.Put_Line ("Client " & Id'Img & " registered");

    Main_Loop:
    for I in 1 .. 5 loop
      My_Io.Put_Line ("Client " & Id'Img & " waiting");
      Dispatcher.Wait (Id);
      My_Io.Put_Line ("Client " & Id'Img & " getting");

      -- Loop on x events (Kind = True) at most 5 X events
      X_Events_Loop:
      for I in 1 .. 5 loop
        Dispatcher.Get(Id) (Kind);
        My_Io.Put_Line ("Client " & Id'Img & " got " & Kind 'Img);
        exit when not Kind;
      end loop X_Events_Loop;

    end loop Main_Loop;
    My_Io.Put_Line ("Client " & Id'Img & " unregistering");
    Dispatcher.Unregister (Id);
    My_Io.Put_Line ("Client " & Id'Img & " exiting");

  exception
    when Error:others =>
      My_Io.Put_Line ("Client " & Id'Img & " EXCEPTION "
         & Ada.Exceptions.Exception_Name (Error));
  end Client;

  Clients : array (Client_Range) of Client;

begin
  null;
exception
  when Error:others =>
    My_Io.Put_Line ("Main EXCEPTION"
         & Ada.Exceptions.Exception_Name (Error));
end T_Dispatcher;

