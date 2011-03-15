with Basic_Proc, Event_Mng, String_Mng, Mixed_Str, As.U, Argument, Async_Stdin;
with Autobus;
procedure T_Autobus is

  Default_Address : constant String := "234.7.6.5:21021";

  procedure Usage is
  begin
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
                                & " <auto> | <manual>");
    Basic_Proc.Put_Line_Output ("   <auto>   ::= --auto [ <message> ]");
    Basic_Proc.Put_Line_Output ("   <manual> ::= --manual <bus_address>");
    Basic_Proc.Put_Line_Output ("Ex: " & Argument.Get_Program_Name
                              & " --manual " & Default_Address);
  end Usage;

  Bus : aliased Autobus.Bus_Type;
  Subs_Repl, Subs_Drop, Subs_Spy, Subs_Put : aliased Autobus.Subscriber_Type;

  -- Signal callback
  Sig : Boolean;
  procedure Signal_Cb is
  begin
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

  -- Observer putting replies in interactive mode
  type Obs_Put_Type is new Autobus.Observer_Type with null record;
  Putter : aliased Obs_Put_Type;
  procedure Receive (Observer : in out Obs_Put_Type;
                     Subscriber : in Autobus.Subscriber_Access_Type;
                     Message : in String) is
    pragma Unreferenced (Observer, Subscriber);
    use type Autobus.Subscriber_Access_Type;
  begin
    Basic_Proc.Put_Line_Output (Message);
  end Receive;

  -- Async stdin callback
  function Async_Cb (Str : String) return Boolean is
    Last : Natural;
  begin
    if Str = "" then
      -- Async stdin error
      Async_Stdin.Put_Line_Err ("Async stdin error");
      Sig := True;
      return True;
    else
      Last  := Str'Last;
      if Str (Last) < ' ' then
        Last  := Last - 1;
      end if;
      if Last > 0 then
        Bus.Send (Str (Str'First .. Last));
      end if;
      return False;
    end if;
  end Async_Cb;


begin
  if Argument.Get_Nbre_Arg = 0 then
    Usage;
    return;
  elsif Argument.Get_Parameter (1) = "--auto" then
    for I in 2 .. Argument.Get_Nbre_Arg loop
      Stimulus.Append (" " & Argument.Get_Parameter (Occurence => I));
    end loop;
    if not Stimulus.Is_Null then
      Stimulus.Delete (1, 1);
    end if;
    Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
    Bus.Init (Default_Address);
    Subs_Repl.Init (Bus'Unrestricted_Access, Receiver'Unrestricted_Access,
                    "Ah que .*", True);
    Subs_Drop.Init (Bus'Unrestricted_Access, Receiver'Unrestricted_Access,
                    "Coucou.*", True);
    Subs_Spy.Init (Bus'Unrestricted_Access, Spy'Unrestricted_Access, "", True);
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
  elsif Argument.Get_Parameter (1) = "--manual"
  and then Argument.Get_Nbre_Arg = 2 then
    Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
    Bus.Init (Argument.Get_Parameter (2));
    Subs_Put.Init (Bus'Unrestricted_Access, Putter'Unrestricted_Access);
    Async_Stdin.Set_Async (Async_Cb'Unrestricted_Access,
                           Autobus.Message_Max_Length);
    loop
      Sig := False;
      Event_Mng.Pause (-1);
      exit when Sig;
    end loop;
    Async_Stdin.Set_Async (null);
    Subs_Put.Reset (Bus'Unrestricted_Access);
    Bus.Reset;
  else
    Usage;
  end if;
end T_Autobus;

