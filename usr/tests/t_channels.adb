with Ada.Text_Io, Ada.Exceptions;
with Argument, Event_Mng, Socket, Timers, Channels;
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
    Ada.Text_Io.Put_Line ("Received message >" & Message.Str(1 .. Length)
                        & "< Len: " & Length'Img
                        & " Diff: " & Diffused'Img);
    -- Reply to diffusion
    if Diffused then
      Ada.Text_Io.Put_Line ("Replying");
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
    Ada.Text_Io.Put_Line ("Aborted.");
    Sig := True;
  end Signal_Cb;

  -- Time callback, no more waiting
  function Timer_Cb (Id : Timers.Timer_Id;
                     Data : Timers.Timer_Data) return Boolean is
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
      Id := Timers.Create ( (Delay_Kind    => Timers.Delay_Sec,
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
    Ada.Text_Io.Put_Line ("Send to " & Host & " -> " & Result'Img);
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
    Ada.Text_Io.Put_Line ("Subscribing");
    begin
      if Bus then
        Fifob.Subscribe;
      else
        Fifoc.Subscribe;
      end if;
    exception
      when Channels.Unknown_Destination =>
        Ada.Text_Io.Put_Line ("Unknown destination");
        return;
    end;
    -- Wait until Nb_To_Do messages received
    Wait (Timers.Infinite_Seconds);

    Ada.Text_Io.Put_Line ("Unsubscribing");
    if Bus then
      Fifob.Unsubscribe;
    else
      Fifoc.Unsubscribe;
    end if;
    -- Wait infinite
    Ada.Text_Io.Put_Line ("Waiting");
    Nb_Done := 0;
    Wait (Timers.Infinite_Seconds);
    return;
  end if;

  -- Publisher
  Message.Str := (others => ' ');
  Message.Str(1 .. 6) := "Coucou";
  if Bus then
    Ada.Text_Io.Put_Line ("Joining");
    Fifob.Join;
  else

    Ada.Text_Io.Put_Line ("Adding dests from file");
    begin
      Fifoc.Add_Destinations ("channels.dir");
    exception
      when Error : others =>
        Ada.Text_Io.Put_Line ("Exception: "
           & Ada.Exceptions.Exception_Name(Error)
           & " raised.");
    end;
    Ada.Text_Io.Put ("Adding dests: ");
    for I in 4 .. Argument.Get_Nbre_Arg loop
      Ada.Text_Io.Put (Argument.Get_Parameter(Occurence => I) & " ");
      begin
        Fifoc.Add_Destination (Argument.Get_Parameter(I));
      exception
        when Channels.Unknown_Destination =>
          Ada.Text_Io.Put_Line ("Unknown destination, skipping");
        when Channels.Destination_Already =>
          Ada.Text_Io.Put_Line ("Destination already set, skipping");
      end;
    end loop;
    Ada.Text_Io.New_Line;
  end if;

  Wait (1.0);
  
  Ada.Text_Io.Put_Line ("Sending " & Nb_To_Do'Img & " messages");
  loop
    Send (Message);
    Nb_Done := Nb_Done + 1;
    exit when Sig;
    exit when Nb_To_Do /= 0 and then Nb_Done = Nb_To_Do;
  end loop;

  Ada.Text_Io.New_Line;

  if Bus then
    for I in 5 .. Argument.Get_Nbre_Arg loop
      Ada.Text_Io.Put_Line ("Sending message to host: "
                     & Argument.Get_Parameter(Occurence => I) );
      begin
        Fifob.Send (Argument.Get_Parameter(I), Message);
      exception
        when Channels.Unknown_Destination =>
          Ada.Text_Io.Put_Line ("Unknown destination");
        when Channels.Send_Failed =>
          Ada.Text_Io.Put_Line ("Send failed");
      end;
      Wait (0.5);
    end loop;
    Ada.Text_Io.New_Line;

    Wait (1.0);
    Ada.Text_Io.Put_Line ("Leaving");
    Fifob.Leave;
  else
    for I in 4 .. Argument.Get_Nbre_Arg-1 loop
      Ada.Text_Io.Put_Line ("Deleting dest "
                          & Argument.Get_Parameter(Occurence => I));
      Fifoc.Del_Destination (Argument.Get_Parameter(I));
      Ada.Text_Io.Put_Line ("Sending one message");
      Send (Message);
    end loop;

    Ada.Text_Io.New_Line;
    Ada.Text_Io.Put_Line ("Deleting all dests and sending one message");
    Fifoc.Del_All_Destinations;
    Send (Message);

    Ada.Text_Io.New_Line;
    Ada.Text_Io.Put_Line ("Sending one message");
    Send (Message);
  end if;

exception
  when Channels.Unknown_Channel =>
    Ada.Text_Io.Put_Line ("Unknown channel "
                        & Argument.Get_Parameter (Occurence => 2));
  when Channels.Unknown_Bus =>
    Ada.Text_Io.Put_Line ("Unknown bus "
                        & Argument.Get_Parameter (Occurence => 2));
  when Channels.Unknown_Destination =>
    Ada.Text_Io.Put_Line ("Unknown destination "
                        & Argument.Get_Parameter (Occurence => 4));
  when Argument.Argument_Not_Found =>
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
        & " <publish_or_subscribe> <channel_name> <nb_messages>"
        & " [ { <dest_of_publisher> } ]");
    Ada.Text_Io.Put_Line ("<publish_or_subscribe> ::= p | s | P | S");
end T_Channels;

