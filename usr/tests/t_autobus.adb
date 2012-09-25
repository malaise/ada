-- In manual mode:
--  * Send on bus the text got by async_stdin
--  * Output on stdout the text received on bus
-- In automatic mode:
--  * Send each second the automatic message if any
--  * Spy (echo "Spyed ><Msg><") each received message, including owers
--  * Drop (echo "Received <Msg> -> Dropping") messages starting by "Coucou"
--  * Reply (echo "Received <Msg> -> Replying <reply>") messages "Ah que "
--  * Reply (echo "Received <Msg> -> Replying <reply>") messages starting by
--   "Ah que " and containing something
--   Reply is the remaining text in Mixed_Str
--  * Exit after sending 3 messages
with Basic_Proc, Event_Mng, Str_Util, Mixed_Str, As.U, Async_Stdin,
     Argument, Argument_Parser;
with Autobus;
procedure T_Autobus is

  Default_Address : constant String := "234.7.6.5:21021";

  procedure Plo (Str : in String) renames Basic_Proc.Put_Line_Output;

  -- Arguments
  Keys : constant Argument_Parser.The_Keys_Type := (
   1 => (False, 'h', As.U.Tus ("help"), False),
   2 => (False, 'a', As.U.Tus ("auto"), False),
   3 => (False, 'm', As.U.Tus ("manual"), False),
   4 => (True, 'b', As.U.Tus ("bus"), False, True, As.U.Tus ("bus_address")));
 Key_Dscr : Argument_Parser.Parsed_Dscr;

  procedure Usage is
  begin
    Plo ("Usage: " & Argument.Get_Program_Name
       & " <mode> [ <bus> ] [ <auto_message> ]");
    Plo ("   or: " & Argument.Get_Program_Name & " "
       & Argument_Parser.Image(Keys(1)));
    Plo (" <mode>   ::= <auto> | <manual>");
    Plo (" <auto>   ::= " & Argument_Parser.Image(Keys(2)));
    Plo (" <manual> ::= " & Argument_Parser.Image(Keys(3)));
    Plo (" <bus>    ::= " & Argument_Parser.Image(Keys(4)));
    Plo ("Ex: " & Argument.Get_Program_Name
       & " --manual -b " & Default_Address);
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("ERROR: " & Msg & ".");
    Basic_Proc.Set_Error_Exit_Code;
  end Error;

  -- Bus and subscribers
  Bus : aliased Autobus.Bus_Type;
  Subs_Repl, Subs_Drop, Subs_Spy, Subs_Put : aliased Autobus.Subscriber_Type;

  -- Signal callback
  Sig : Boolean;
  procedure Signal_Cb is
  begin
    Sig := True;
  end Signal_Cb;

  Stimulus : As.U.Asu_Us;

  -- Observer receiver of messages
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
    Index := Str_Util.Locate (Message, " ", Occurence => 2);
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
    Async_Stdin.Put_Out (Message);
    Async_Stdin.Flush_Out;
  end Receive;

  -- Async stdin callback
  function Async_Cb (Str : String) return Boolean is
  begin
    if Str = "" then
      -- Async stdin error
      Async_Stdin.Put_Line_Err ("Async stdin error");
      Sig := True;
      return True;
    else
      Bus.Send (Str);
      return False;
    end if;
  end Async_Cb;


begin

  -- Parse arguments
  Key_Dscr :=  Argument_Parser.Parse (Keys);
  if not Key_Dscr.Is_Ok then
    Error ("Invalid arguments, " & Key_Dscr.Get_Error);
    Usage;
    return;
  end if;
  if Key_Dscr.Get_Nb_Embedded_Arguments /= 0 then
    Error ("Invalid arguments");
    Usage;
    return;
  end if;

  -- Help
  if Key_Dscr.Is_Set (1) then
    Usage;
    return;
  end if;

  -- Mode required
  if not Key_Dscr.Is_Set (2) and then not Key_Dscr.Is_Set (3) then
    Error ("Missing mode");
    Usage;
    return;
  end if;

  -- No message in manual mode
  if  Key_Dscr.Is_Set (3)
  and then Key_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 0 then
    Error ("No automatic message allowed in manual mode");
    Usage;
    return;
  end if;

  -- Init bus
  if Key_Dscr.Is_Set (4) then
    if Key_Dscr.Get_Option (4, 1) = "" then
      Error ("Missing bus address");
    end if;
    Bus.Init (Key_Dscr.Get_Option (4, 1));
  else
    Bus.Init (Default_Address);
  end if;

  if Key_Dscr.Is_Set (2) then
    -- Automatic mode
    -- Concat parts of automatic message
    for I in 1 .. Key_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) loop
      Stimulus.Append (" " & Key_Dscr.Get_Option (
                              Argument_Parser.No_Key_Index, I));
    end loop;
    if not Stimulus.Is_Null then
      Stimulus.Delete (1, 1);
    end if;
    Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
    Subs_Repl.Init (Bus'Unrestricted_Access, Receiver'Unrestricted_Access,
                    "Ah que .*", False);
    Subs_Drop.Init (Bus'Unrestricted_Access, Receiver'Unrestricted_Access,
                    "Coucou.*", False);
    Subs_Spy.Init (Bus'Unrestricted_Access, Spy'Unrestricted_Access, "", True);
    if Stimulus.Is_Null then
      loop
        Sig := False;
        Event_Mng.Pause (10_000);
        exit when Sig;
      end loop;
    else
      for I in 1 .. 3 loop
        Sig := False;
        if not Stimulus.Is_Null then
          Basic_Proc.Put_Line_Output ("Sending " & Stimulus.Image);
          Bus.Send (Stimulus.Image);
        end if;
        Event_Mng.Pause (1_000);
        exit when Sig;
      end loop;
    end if;

    Basic_Proc.Put_Line_Output ("Closing.");
    Subs_Repl.Reset;
    Bus.Reset;
    Basic_Proc.Put_Line_Output ("Done.");
  else
    -- Manual mode
    Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
    Subs_Put.Init (Bus'Unrestricted_Access, Putter'Unrestricted_Access);
    Async_Stdin.Set_Async (Async_Cb'Unrestricted_Access,
                           Autobus.Message_Max_Length);
    loop
      Sig := False;
      Event_Mng.Pause (-1);
      exit when Sig;
    end loop;
    Async_Stdin.Set_Async;
    Subs_Put.Reset;
    Bus.Reset;
  end if;
end T_Autobus;

