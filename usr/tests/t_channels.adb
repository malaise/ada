with Ada.Exceptions;
with Basic_Proc, Argument, Event_Mng, Socket, Timers, Channels;
procedure T_Channels is

  -- Channel, bus, dest name
  Channel_Name : constant String := "dummy_channel";
  Bus_Name : constant String := "dummy_bus";
  Destination_Name : constant String := "dummy_dest";

  -- Message exchanged
  type Message_Type is record
    Str : String (1 .. 50);
  end record;

  -- Are we publisher or subscriber
  Publish : Boolean;

  -- Is it a bus
  Bus : Boolean;

  -- Nb messages to process and processed
  Nb_To_Do  : Natural;
  Nb_Done : Natural;

  -- The Fifo
  procedure Fifo_Cb (Message  : in Message_Type;
                     Length   : in Channels.Message_Length;
                     Diffused : in Boolean);

  package Fifoc is new Channels.Channel (Channel_Name, Message_Type, Fifo_Cb);
  package Fifob is new Channels.Bus (Bus_Name, Destination_Name,
                                     Message_Type, Fifo_Cb);

  -- Reception callback
  procedure Fifo_Cb (Message  : in Message_Type;
                     Length   : in Channels.Message_Length;
                     Diffused : in Boolean) is
    Msg : Message_Type;
  begin
    Basic_Proc.Put_Line_Output ("Received message >" & Message.Str(1 .. Length)
                        & "< Len: " & Length'Img
                        & " Diff: " & Diffused'Img);
    -- Reply to diffusion
    if Diffused then
      Basic_Proc.Put_Line_Output ("Replying");
      declare
        Name : constant String := Socket.Local_Host_Name;
      begin
        Msg.Str (1 .. Name'Length) := Name;
        if Bus then
          Fifob.Reply (Msg, Name'Length);
        else
          Fifoc.Reply (Msg, Name'Length);
        end if;
      end;
    end if;
    -- Count subscriber receptions
    if not Publish and then Nb_To_Do /= 0 then
      Nb_Done := Nb_Done + 1;
    end if;
  end Fifo_Cb;

  -- Go on waiting
  Go_Wait : Boolean;

  -- Signal received
  Sig : Boolean := False;

  -- Signal callback
  procedure Signal_Cb is
  begin
    Basic_Proc.Put_Line_Output ("Aborted.");
    Sig := True;
  end Signal_Cb;

  -- Time callback, no more waiting
  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
    pragma Unreferenced (Id, Data);
  begin
    Go_Wait := False;
    return True;
  end Timer_Cb;

  -- Infinite wait or use a timer
  procedure Wait (Dur : in Duration) is
    Id : Timers.Timer_Id;
  begin
    if Sig then
      return;
    end if;
    Go_Wait := True;
    if Dur /= Timers.Infinite_Seconds then
      -- No timer if infinite
      Id.Create ( (Delay_Kind    => Timers.Delay_Sec,
                   Clock         => null,
                   Period        => Timers.No_Period,
                   Delay_Seconds => Dur),
                     Timer_Cb'Unrestricted_Access);
    end if;
    loop
      Event_Mng.Wait (Integer (Dur) * 1_000);
      exit when Sig;
      -- Stops waiting on timer
      exit when not Go_Wait;
      -- Subscriber stops waiting when amount of messages received
      exit when not Publish
                and then Nb_To_Do /= 0
                and then Nb_Done = Nb_To_Do;
    end loop;
  end Wait;

  procedure Send_Cb (Host : in String; Result : in Boolean) is
  begin
    Basic_Proc.Put_Line_Output ("Send to " & Host & " -> " & Result'Img);
  end Send_Cb;

  procedure Send (Msg : in Message_Type) is
  begin
    if Bus then
      Fifob.Write (Msg);
    else
      Fifoc.Write (Msg, Send_Cb => Send_Cb'Unrestricted_Access);
    end if;
    Wait (0.5);
  end Send;

  -- Message sent
  Message : Message_Type;

begin

  if Argument.Get_Parameter = "p" then
    Publish := True;
    Bus := False;
  elsif Argument.Get_Parameter = "s" then
    Publish := False;
    Bus := False;
  elsif Argument.Get_Parameter = "P" then
    Publish := True;
    Bus := True;
  elsif Argument.Get_Parameter = "S" then
    Publish := False;
    Bus := True;
  else
    raise Argument.Argument_Not_Found;
  end if;

  if Bus then
    Fifob.Change_Names (Argument.Get_Parameter(2), Argument.Get_Parameter(4));
  else
    Fifoc.Change_Channel_Name (Argument.Get_Parameter(2));
  end if;
  Nb_To_Do := Positive'Value (Argument.Get_Parameter(3));
  Nb_Done := 0;
  Event_Mng.Set_Sig_Term_Callback (Signal_Cb'Unrestricted_Access);

  if not Publish then
    if (Bus and then Argument.Get_Nbre_Arg /= 4)
    or else (not Bus and then Argument.Get_Nbre_Arg /= 3) then
      -- One dest on Bus, No dest on Channel
      raise Argument.Argument_Not_Found;
    end if;
    -- Subscriber
    Basic_Proc.Put_Line_Output ("Subscribing");
    begin
      if Bus then
        Fifob.Subscribe;
      else
        Fifoc.Subscribe;
      end if;
    exception
      when Channels.Unknown_Destination =>
        Basic_Proc.Put_Line_Output ("Unknown destination");
        return;
    end;
    -- Wait until Nb_To_Do messages received
    Wait (Timers.Infinite_Seconds);

    Basic_Proc.Put_Line_Output ("Unsubscribing");
    if Bus then
      Fifob.Unsubscribe;
    else
      Fifoc.Unsubscribe;
    end if;
    -- Wait infinite
    Basic_Proc.Put_Line_Output ("Waiting");
    Nb_Done := 0;
    Wait (Timers.Infinite_Seconds);
    return;
  end if;

  -- Publisher
  Message.Str := (others => ' ');
  Message.Str(1 .. 6) := "Coucou";
  if Bus then
    Basic_Proc.Put_Line_Output ("Joining");
    Fifob.Join;
  else

    Basic_Proc.Put_Line_Output ("Adding dests from file");
    begin
      Fifoc.Add_Destinations ("channels.xml");
    exception
      when Error : others =>
        Basic_Proc.Put_Line_Output ("Exception: "
           & Ada.Exceptions.Exception_Name(Error)
           & " raised.");
    end;
    Basic_Proc.Put_Output ("Adding dests: ");
    for I in 4 .. Argument.Get_Nbre_Arg loop
      Basic_Proc.Put_Output (Argument.Get_Parameter(Occurence => I) & " ");
      begin
        Fifoc.Add_Destination (Argument.Get_Parameter(I));
      exception
        when Channels.Unknown_Destination =>
          Basic_Proc.Put_Line_Output ("Unknown destination, skipping");
        when Channels.Destination_Already =>
          Basic_Proc.Put_Line_Output ("Destination already set, skipping");
      end;
    end loop;
    Basic_Proc.New_Line_Output;
  end if;

  Wait (1.0);

  Basic_Proc.Put_Line_Output ("Sending " & Nb_To_Do'Img & " messages");
  loop
    Send (Message);
    Nb_Done := Nb_Done + 1;
    exit when Sig;
    exit when Nb_To_Do /= 0 and then Nb_Done = Nb_To_Do;
  end loop;

  Basic_Proc.New_Line_Output;

  if Bus then
    for I in 5 .. Argument.Get_Nbre_Arg loop
      Basic_Proc.Put_Line_Output ("Sending message to host: "
                     & Argument.Get_Parameter(Occurence => I) );
      begin
        Fifob.Send (Argument.Get_Parameter(I), Message);
      exception
        when Channels.Unknown_Destination =>
          Basic_Proc.Put_Line_Output ("Unknown destination");
        when Channels.Send_Failed =>
          Basic_Proc.Put_Line_Output ("Send failed");
      end;
      Wait (0.5);
    end loop;
    Basic_Proc.New_Line_Output;

    Wait (1.0);
    Basic_Proc.Put_Line_Output ("Leaving");
    Fifob.Leave;
  else
    for I in 4 .. Argument.Get_Nbre_Arg-1 loop
      Basic_Proc.Put_Line_Output ("Deleting dest "
                          & Argument.Get_Parameter(Occurence => I));
      Fifoc.Del_Destination (Argument.Get_Parameter(I));
      Basic_Proc.Put_Line_Output ("Sending one message");
      Send (Message);
    end loop;

    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Deleting all dests and sending one message");
    Fifoc.Del_All_Destinations;
    Send (Message);

    Basic_Proc.New_Line_Output;
    Basic_Proc.Put_Line_Output ("Sending one message");
    Send (Message);
  end if;

exception
  when Channels.Unknown_Channel =>
    Basic_Proc.Put_Line_Output ("Unknown channel "
                        & Argument.Get_Parameter (Occurence => 2));
  when Channels.Unknown_Bus =>
    Basic_Proc.Put_Line_Output ("Unknown bus "
                        & Argument.Get_Parameter (Occurence => 2));
  when Channels.Unknown_Destination =>
    Basic_Proc.Put_Line_Output ("Unknown destination "
                        & Argument.Get_Parameter (Occurence => 4));
  when Argument.Argument_Not_Found =>
    Basic_Proc.Put_Line_Output ("Usage: " & Argument.Get_Program_Name
        & " <publish_or_subscribe> <channel_name> <nb_messages>"
        & " [ { <dest_of_publisher> } ]");
    Basic_Proc.Put_Line_Output ("<publish_or_subscribe> ::= p | s | P | S");
end T_Channels;

