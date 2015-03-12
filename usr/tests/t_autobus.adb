-- In manual mode:
--  * Send on bus the text got by async_stdin
--  * if <send> option, recognises "sendto <addr> text" and sends <text> to <addr>
--  * Output on stdout the text received on bus
-- In automatic mode create an Active reliable bus and:
--  * Send each second the automatic message, if provided in command line
--  * Spy (echo "Spyed ><Msg><") each received message, including owers
--  * Drop (echo "Received <Msg> -> Dropping") the messages received by Drop
--    observer (i.e. matching "Coucou.*"
--  * In Repl observer (messages matching "Ah que .*")
--   - Reject (echo "Received <Msg> -> Rejecting") messages that contain less
--     than '2 spaces then a letter'
--   - Reply (echo "Received <Msg> -> Replying <reply>") other messages
--     Reply is the remaining text after second space, in Mixed_Str
--  * Exit after sending 3 messages
with Ada.Exceptions;
with Basic_Proc, Event_Mng, Str_Util, Mixed_Str, As.U, Async_Stdin,
     Argument, Argument_Parser, Trilean, Aski, Regular_Expressions;
with Autobus;
procedure T_Autobus is

  Default_Address : constant String := "234.7.6.5:21021";
  Bus_Address : As.U.Asu_Us;

  procedure Plo (Str : in String) renames Basic_Proc.Put_Line_Output;
  procedure Ple (Str : in String) renames Basic_Proc.Put_Line_Error;

  -- Arguments
  Keys : constant Argument_Parser.The_Keys_Type := (
   1 => (False, 'h', As.U.Tus ("help"), False),
   2 => (False, 'a', As.U.Tus ("auto"), False),
   3 => (False, 'A', As.U.Tus ("active"), False),
   4 => (False, 'P', As.U.Tus ("passive"), False),
   5 => (False, 'M', As.U.Tus ("multicast"), False),
   6 => (True, 'b', As.U.Tus ("bus"), False, True, As.U.Tus ("bus_address")),
   7 => (False, 's', As.U.Tus ("send"), False),
   8 => (False, 'n', As.U.Tus ("nolf"), False) );
 Key_Dscr : Argument_Parser.Parsed_Dscr;

  procedure Usage is
  begin
    Plo ("Usage: " & Argument.Get_Program_Name
       & " <mode> [ <bus> ] [ <auto_message> ]");
    Plo ("   or: " & Argument.Get_Program_Name & " "
       & Argument_Parser.Image(Keys(1)));
    Plo (" <mode>      ::= <auto> | <manual>");
    Plo (" <auto>      ::= " & Argument_Parser.Image(Keys(2)));
    Plo (" <manual>    ::= <active> | <passive> | <multicast>  [ <send> ] [ <nolf> ");
    Plo (" <active>    ::= " & Argument_Parser.Image(Keys(3)));
    Plo (" <passive>   ::= " & Argument_Parser.Image(Keys(4)));
    Plo (" <multicast> ::= " & Argument_Parser.Image(Keys(5)));
    Plo (" <send>      ::= " & Argument_Parser.Image(Keys(7)));
    Plo (" <nolf>      ::= " & Argument_Parser.Image(Keys(8)));
    Plo (" <bus>    ::= " & Argument_Parser.Image(Keys(6)));
    Plo ("Ex: " & Argument.Get_Program_Name
       & " --active -b " & Default_Address);
    Plo ("Send mode leads to detect ""sendto <addr> <text>"" and to send text to addr.");
  end Usage;

  procedure Error (Msg : in String) is
  begin
    Ple ("ERROR: " & Msg & ".");
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

  -- Supervision callback
  procedure Sup_Cb (Report : in Autobus.Sup_Report) is
    use type Trilean.Trilean;
  begin
    Ple ((case Report.State is
            when Trilean.True  => "Insertion of: ",
            when Trilean.False => "Death of: ",
            when Trilean.Other => "Own address: ") & Report.Addr.Image);
  end Sup_Cb;

  Send_Mode : Boolean;
  Nolf : Boolean;
  Send_Str : constant String := "sendto";
  Stimulus : As.U.Asu_Us;
  Nb_Opt : Natural;

  -- Observer receiver of messages
  type Obs_Rece_Type is new Autobus.Observer_Type with null record;
  Receiver : aliased Obs_Rece_Type;
  procedure Receive (Unused_Observer : in out Obs_Rece_Type;
                     Subscriber      : in Autobus.Subscriber_Access_Type;
                     Message         : in String) is
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
    Bus.Reply (Str.Image);
  end Receive;

  -- Observer spying of messages
  type Obs_Spy_Type is new Autobus.Observer_Type with null record;
  Spy : aliased Obs_Spy_Type;
  procedure Receive (Unused_Observer   : in out Obs_Spy_Type;
                     Unused_Subscriber : in Autobus.Subscriber_Access_Type;
                     Message           : in String) is
    use type Autobus.Subscriber_Access_Type;
  begin
    Basic_Proc.Put_Line_Output ("Spyed >" & Message & "<");
  end Receive;

  -- Observer putting replies in interactive mode
  type Obs_Put_Type is new Autobus.Observer_Type with null record;
  Putter : aliased Obs_Put_Type;
  procedure Receive (Unused_Observer   : in out Obs_Put_Type;
                     Unused_Subscriber : in Autobus.Subscriber_Access_Type;
                     Message           : in String) is
    use type Autobus.Subscriber_Access_Type;
  begin
    Async_Stdin.Put_Out (Message);
    if Nolf
    and then (Message = "" or else Message (Message'Last) /= Aski.Lf) then
      Async_Stdin.New_Line_Out;
    end if;
    Async_Stdin.Flush_Out;
  end Receive;

  -- Async stdin callback
  function Async_Cb (Str : String) return Boolean is
    Do_Sendto : Boolean;
    Addr, Msg : As.U.Asu_Us;
  begin
    if Str = "" then
      -- Async stdin error
      Async_Stdin.Put_Line_Err ("Async stdin error");
      Sig := True;
      return True;
    end if;

    Msg := As.U.Tus (Str);
    if Nolf and then Msg.Length > 1
    and then Msg.Element (Msg.Length) = Aski.Lf then
      -- Remove trailing Lf except if empty line
      Msg.Trail (1);
    end if;

    -- In Send mode, check validity of request
    -- "sendto <addr> <msg>
    Do_Sendto := Send_Mode;
    if Do_Sendto then
      declare
        Matches : constant Regular_Expressions.Match_Array
                := Regular_Expressions.Match (
                     Send_Str & " +([^ ]+) +(.+)", Msg.Image, 3);
      begin
        -- Check that string matches
        Do_Sendto := Matches'Length = 3
            and then Regular_Expressions.Strict_Match (Msg.Image, Matches(1))
            and then Regular_Expressions.Valid_Match (Matches(1));
        if Do_Sendto then
          Addr := Msg.Uslice (Matches(2).First_Offset,
                              Matches(2).Last_Offset_Stop);
          Msg := Msg.Uslice (Matches(3).First_Offset,
                             Matches(3).Last_Offset_Stop);
        end if;
      end;
    end if;

    if Do_Sendto then
      Bus.Send_To (Addr.Image, Msg.Image);
     else
      Bus.Send (Msg.Image);
    end if;
    return False;
  exception
    when Error:others =>
      Async_Stdin.Put_Line_Err ("Exception "
          & Ada.Exceptions.Exception_Name (Error) & " raised");
      return False;
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

  -- One mode required
  Nb_Opt := 0;
  if Key_Dscr.Is_Set (2) then
    Nb_Opt := Nb_Opt + 1;
  end if;
  if Key_Dscr.Is_Set (3) then
    Nb_Opt := Nb_Opt + 1;
  end if;
  if Key_Dscr.Is_Set (4) then
    Nb_Opt := Nb_Opt + 1;
  end if;
  if Key_Dscr.Is_Set (5) then
    Nb_Opt := Nb_Opt + 1;
  end if;
  if Nb_Opt = 0 then
    Error ("Missing mode");
    Usage;
    return;
  elsif Nb_Opt > 1 then
    Error ("Too many modes");
    Usage;
    return;
  end if;

  -- No message in manual mode
  if (Key_Dscr.Is_Set (3) or else Key_Dscr.Is_Set (4)
      or else Key_Dscr.Is_Set (5))
  and then Key_Dscr.Get_Nb_Occurences (Argument_Parser.No_Key_Index) /= 0 then
    Error ("No automatic message allowed in manual mode");
    Usage;
    return;
  end if;

  -- No send mode nor nolf in auto
  if Key_Dscr.Is_Set (2) then
    if Key_Dscr.Is_Set (7) then
      Error ("No send option in automatic mode");
      Usage;
      return;
    end if;
    if Key_Dscr.Is_Set (8) then
      Error ("No nolf option in automatic mode");
      Usage;
      return;
    end if;
  end if;


  -- Init bus with address provided or default
  --  with supervision callback in manual
  if Key_Dscr.Is_Set (6) then
    if Key_Dscr.Get_Option (6, 1) = "" then
      Error ("Missing bus address");
    end if;
    Bus_Address := As.U.Tus (Key_Dscr.Get_Option (6, 1));
  else
    Bus_Address := As.U.Tus (Default_Address);
  end if;

  if Key_Dscr.Is_Set (2) then
    -- Automatic mode
    -- Init Bus, Active and no Cb
    Bus.Init (Bus_Address.Image, Autobus.Active);
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
    -- Send mode and nolf options?
    Send_Mode := Key_Dscr.Is_Set (7);
    Nolf := Key_Dscr.Is_Set (8);
    -- Init bus in Active, Passive or Multicast. With Cb
    Bus.Init (Bus_Address.Image,
              (if Key_Dscr.Is_Set (3) then Autobus.Active
               elsif Key_Dscr.Is_Set (4) then Autobus.Passive
               else Autobus.Multicast),
              Sup_Cb'Unrestricted_Access);
    Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);
    -- Init subscriber
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

