with Ada.Text_Io, Ada.Exceptions;
with Argument, X_Mng, Socket, Timers, Channels;
procedure T_Channels is

  -- Channel name
  Channel_Name : constant String := "dummy_channel";

  -- Message exchanged
  type Message_Type is record
    Str : String (1 .. 50);
  end record;

  -- Are we publisher or subscriber
  Publish : Boolean;

  -- Nb messages to process and processed
  Nb_To_Do  : Natural;
  Nb_Done : Natural;

  -- The Fifo
  procedure Fifo_Cb (Message  : in Message_Type;
                     Length   : in Channels.Message_Length;
                     Diffused : in Boolean);

  package Fifo is new Channels.Channel (Channel_Name, Message_Type, Fifo_Cb);

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
        Fifo.Reply (Msg, Name'Length);
      end;
    end if;
    -- Count subscriber receptions
    if not Publish and then Nb_To_Do /= 0 then
      Nb_Done := Nb_Done + 1;
    end if;
  end Fifo_Cb;

  -- Go on waiting
  Go_Wait : Boolean;

  -- Time callback, no more waiting
  function Timer_Cb (Id : in Timers.Timer_Id) return Boolean is
  begin
    Go_Wait := False;
    return True;
  end Timer_Cb;

  -- Infinite wait or use a timer
  procedure Wait (Dur : in Duration) is
    Id : Timers.Timer_Id;
    Event : Boolean;
  begin
    Go_Wait := True;
    if Dur /= Timers.Infinite_Seconds then
      -- No timer if infinite
      Id := Timers.Create ( (Delay_Kind    => Timers.Delay_Sec,
                             Period        => Timers.No_Period,
                             Delay_Seconds => Dur),
                             Timer_Cb'Unrestricted_Access);
    end if;
    loop
      Event := X_Mng.Select_No_X (Integer (Dur) * 1_000);
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

  procedure Send (Msg : in Message_type) is
  begin
    Fifo.Write (Msg, Send_Cb => Send_Cb'Unrestricted_Access);
    Wait (0.5);
  end Send;

  -- Message sent
  Message : Message_type;

begin

  if Argument.Get_Parameter = "p" then
    Publish := True;
  elsif Argument.Get_Parameter = "s" then
    Publish := False;
  else
    return;
  end if;
  Fifo.Change_Channel_Name (Argument.Get_Parameter(2));
  Nb_To_Do := Positive'Value (Argument.Get_Parameter(3));
  Nb_Done := 0;

  if not Publish then
    -- Subscriber
    Ada.Text_Io.Put_Line ("Subscribing");
    begin
      Fifo.Subscribe;
    exception
      when Channels.Unknown_Destination =>
        Ada.Text_Io.Put_Line ("Unknown destination");
        return;
    end;
    -- Wait until Nb_To_Do messages received
    Wait (Timers.Infinite_Seconds);

    Ada.Text_Io.Put_Line ("Unsubscribing");
    Fifo.Unsubscribe;
    -- Wait infinite
    Nb_Done := 0;
    Wait (Timers.Infinite_Seconds);
  end if;

  -- Publisher
  Message.Str := (others => ' ');
  Message.Str(1 .. 6) := "Coucou";
  Ada.Text_Io.Put_Line ("Adding dests from file");
  begin
    Fifo.Add_Destinations ("channels.dir");
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
      Fifo.Add_Destination (Argument.Get_Parameter(I));
    exception
      when Channels.Unknown_Destination =>
        Ada.Text_Io.Put_Line ("Unknown destination, skipping");
      when Channels.Destination_Already =>
        Ada.Text_Io.Put_Line ("Destination already set, skipping");
    end;
  end loop;
  Ada.Text_Io.New_Line;
  Wait (1.0);
  
  Ada.Text_Io.Put_Line ("Sending " & Nb_To_Do'Img & " messages");
  loop
    Send (Message);
    Nb_Done := Nb_Done + 1;
    exit when Nb_To_Do /= 0 and then Nb_Done = Nb_To_Do;
  end loop;

  Ada.Text_Io.New_Line;
  for I in 4 .. Argument.Get_Nbre_Arg-1 loop
    Ada.Text_Io.Put_Line ("Deleting dest "
                        & Argument.Get_Parameter(Occurence => I));
    Fifo.Del_Destination (Argument.Get_Parameter(I));
    Ada.Text_Io.Put_Line ("Sending one message");
    Send (Message);
  end loop;

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Deleting all dests and sending one message");
  Fifo.Del_All_Destinations;
  Send (Message);

  Ada.Text_Io.New_Line;
  Ada.Text_Io.Put_Line ("Sending one message");
  Send (Message);
exception
  when Channels.Unknown_Channel =>
    Ada.Text_Io.Put_Line ("Unknown channel "
                        & Argument.Get_Parameter (Occurence => 2));
  when Argument.Argument_Not_Found =>
    Ada.Text_Io.Put_Line ("Usage: " & Argument.Get_Program_Name
        & " <publish_or_subscribe> <channel_name> <nb_messages>"
        & " [ { <dest_of_publisher> } ]");
    Ada.Text_Io.Put_Line ("<publish_or_subscribe> ::= p | s");
end T_Channels;

