with Ada.Exceptions;
with Autobus, Timers, Event_Mng, Basic_Proc;
with Utils;
package body Communication is

  -- Signal management
  Sig : Boolean := False;
  procedure Signal_Cb is
  begin
    Sig := True;
  end Signal_Cb;
  function Sig_Received return Boolean is
  begin
    return Sig;
  end Sig_Received;

  -- General state
  Server : Boolean;
  Connected : Boolean := False;

  -- Bus management
  Bus : aliased Autobus.Bus_Type;
  Subs : aliased Autobus.Subscriber_Type;

  -- Connection Cb
  type Connect_Type is new Autobus.Observer_Type with null record;
  procedure Receive (Observer : in out Connect_Type;
                     Subscriber : in Autobus.Subscriber_Access_Type;
                     Message : in String) is
    pragma Unreferenced (Observer, Subscriber);
  begin
    if Utils.Debug_Comm then
      Utils.Debug ("Received " & Message);
    end if;
    if Message = "E" then
      raise Utils.Abort_Game;
    end if;
    if Server and then Message = "C" then
      -- Server receiving a client login
      Bus.Send ("Z");
      Connected := True;
    elsif not Server and then Message = "Z" then
      -- Client receiving a server reset
      Connected := True;
    end if;
  end Receive;
  Connector : aliased Connect_Type;

  -- Timer management
  Tid : Timers.Timer_Id;
  function Timer (Id : Timers.Timer_Id; Data : in Timers.Timer_Data)
                 return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Bus.Send ("C");
    return False;
  end Timer;

  -- Wait until a partner is connected
  -- When client, send a "C" (client) each second
  procedure Connect (Addr : in String; Server : in Boolean) is
  begin
    -- Init signal
    Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Access);

    -- Init Bus
    begin
      Bus.Init (Addr);
    exception
      when Error:others =>
        Basic_Proc.Put_Line_Output ("Cannot init Bus at address " & Addr
            & ", exception: " & Ada.Exceptions.Exception_Name (Error));
        raise Init_Error;
    end;
    if Utils.Debug_Comm then
      Utils.Debug ("Bus initialised");
    end if;

    -- Init connection observer
    Communication.Server := Server;
    Subs.Init (Bus'Access, Connector'Access, "");

    -- Init client timer
    if not Server then
      Tid.Create ( (Delay_Kind => Timers.Delay_Sec,
                    Clock => null,
                    Period => 1.0,
                    Delay_Seconds => 1.0),
                   Timer'Access);
    end if;
  end Connect;

  -- Are we connected? if yes, stop connecting
  function Is_Connected return Boolean is
  begin
    if Connected then
      -- Done, cancel timer if client and cancel observer
      if not Server then
        Tid.Delete;
      end if;
      Subs.Reset;
      if Utils.Debug_Comm then
        Utils.Debug ("Connection completed");
      end if;
    end if;
    return Connected;
  end Is_Connected;

  -- Set reception callback
  Reception : Reception_Cb := null;
  type Reception_Type is new Autobus.Observer_Type with null record;
  procedure Receive (Observer : in out Reception_Type;
                     Subscriber : in Autobus.Subscriber_Access_Type;
                     Message : in String) is
    pragma Unreferenced (Observer, Subscriber);
  begin
    if Utils.Debug_Comm then
      Utils.Debug ("Received " & Message);
    end if;
    if Reception /= null then
      Reception (Message);
    end if;
  end Receive;
  Receptor : aliased Reception_Type;
  procedure Set_Callback (Receive : Reception_Cb := null) is
  begin
    if Subs.Is_Init then
      Subs.Reset;
    end if;
    if Receive /= null then
      Reception := Receive;
      Subs.Init (Bus'Access, Receptor'Access, "");
    end if;
  end Set_Callback;

  -- Send "E" (end) to partner
  procedure Send_End is
  begin
    Send ("E");
  end Send_End;

  -- Send a message to partner
  procedure Send (Msg : in String) is
  begin
    if Utils.Debug_Comm then
      Utils.Debug ("Sending >" & Msg & "<");
    end if;
    Bus.Send (Msg);
  end Send;

  procedure Close is
  begin
    if Utils.Debug_Comm then
      Utils.Debug ("Closing communications");
    end if;
    if Subs.Is_Init then
      Subs.Reset;
    end if;
    if Bus.Is_Init then
      Bus.Reset;
    end if;
  end Close;

end Communication;

