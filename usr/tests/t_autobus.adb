with Basic_Proc, Event_Mng, String_Mng, Mixed_Str, As.U, Argument;
with Autobus;
procedure T_Autobus is

  Bus : aliased Autobus.Bus_Type;
  Subs_Repl, Subs_Drop, Subs_Spy : aliased Autobus.Subscriber_Type;

  -- Signal callback
  Sig : Boolean;
  procedure Signal_Cb is
  begin
    Basic_Proc.Put_Line_Output ("Signal.");
    Sig := True;
  end Signal_Cb;

  Stimulus : As.U.Asu_Us;

  -- Observer recevier of messages
  type Obs_Rece_Type is new Autobus.Observer_Type with null record;
  Receiver : aliased Obs_Rece_Type;
  procedure Receive (Observer : in out Obs_Rece_Type;
                     Subscriber : in Autobus.Subscriber_Access_Type;
                     Message : in String) is
    pragma Unreferenced (Observer);
    Index : Natural;
    Str : As.U.Asu_Us;
    use type Autobus.Subscriber_Access_Type;
  begin
    Basic_Proc.Put_Output ("Received " & Message);
    Basic_Proc.Flush_Output;
    if Subscriber = Subs_Drop'Unrestricted_Access then
      Basic_Proc.Put_Line_Output (" -> Dropping");
      -- No reply
      return;
    end if;
    Index := String_Mng.Locate (Message, " ", Occurence => 2);
    if Index = 0  or else Index = Message'Last then
      Basic_Proc.Put_Line_Output (" -> Rejecting");
      return;
    end if;
    Str := As.U.Tus (Mixed_Str (Message(Index + 1 .. Message'Last) & '!'));
    Basic_Proc.Put_Line_Output (" -> Replying " & Str.Image);
    Bus.Send (Str.Image);
  end Receive;

  -- Observer spying of messages
  type Obs_Spy_Type is new Autobus.Observer_Type with null record;
  Spy : aliased Obs_Spy_Type;
  procedure Receive (Observer : in out Obs_Spy_Type;
                     Subscriber : in Autobus.Subscriber_Access_Type;
                     Message : in String) is
    pragma Unreferenced (Observer, Subscriber);
    use type Autobus.Subscriber_Access_Type;
  begin
    Basic_Proc.Put_Line_Output ("Spyed >" & Message & "<");
  end Receive;

begin
  for I in 1 .. Argument.Get_Nbre_Arg loop
    Stimulus.Append (" " & Argument.Get_Parameter (Occurence => I));
  end loop;
  if not Stimulus.Is_Null then
    Stimulus.Delete (1, 1);
  end if;
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
  Bus.Init ("234.7.6.5:21021");
  Subs_Repl.Init (Bus'Unrestricted_Access, "Ah que .*",
                  Receiver'Unrestricted_Access);
  Subs_Drop.Init (Bus'Unrestricted_Access, "Coucou.*",
                  Receiver'Unrestricted_Access);
  Subs_Spy.Init (Bus'Unrestricted_Access, "",
                 Spy'Unrestricted_Access);
  loop
    Sig := False;
    if not Stimulus.Is_Null then
      Basic_Proc.Put_Line_Output ("Sending " & Stimulus.Image);
      Bus.Send (Stimulus.Image);
    end if;
    Event_Mng.Pause (10_000);
    exit when Sig;
  end loop;

  Basic_Proc.Put_Line_Output ("Closing.");
  Subs_Repl.Reset (Bus'Unrestricted_Access);
  Bus.Reset;
  Basic_Proc.Put_Line_Output ("Done.");
end T_Autobus;

