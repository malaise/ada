with Basic_Proc, Event_Mng, String_Mng, Mixed_Str;
with Autobus;
procedure T_Autobus is
  Bus : aliased Autobus.Bus_Type;
  Subs : Autobus.Subscriber_Type;

  -- Signal callback
  Sig : Boolean;
  procedure Signal_Cb is
  begin
    Basic_Proc.Put_Line_Output ("Signal.");
    Sig := True;
  end Signal_Cb;

  -- Observer of messages
  type Obs_Type is new Autobus.Observer_Type with null record;
  procedure Bus_Receive (Observer : in out Obs_Type; Message : in String) is
    pragma Unreferenced (Observer);
    Index : Natural;
  begin
    Index := String_Mng.Locate (Message, " ", Occurence => 2);
    if Index = 0  or else Index = Message'Last then
      return;
    end if;
    Bus.Send (Mixed_Str (Message(Index + 1 .. Message'Last) & '!'));
  end Bus_Receive;
  Obs : aliased Obs_Type;

begin
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  Bus.Init ("234.7.6.5:21021");
  Subs.Init (Bus'Unrestricted_Access, "Ah.*", Obs'Unrestricted_Access);

  loop
    Sig := False;
    Event_Mng.Pause (10_000);
    exit when Sig;
    Bus.Send ("Ah que coucou");
  end loop;

  Basic_Proc.Put_Line_Output ("Done.");
end T_Autobus;

