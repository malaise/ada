with Ada.Text_Io;
with Argument, X_Mng, Socket, Timers, Channels;
procedure T_Channels is

  -- Message exchanged
  type Message_Type is record
    Str : String (1 .. 50);
  end record;

  -- Are we publisher or subscriber
  Publish : Boolean;

  -- Nb messages to process and processed
  Nb_To_Do  : Positive;
  Nb_Done : Natural;

  -- The Fifo
  procedure Fifo_Cb (Message  : in Message_Type;
                     Length   : in Channels.Message_Length;
                     Diffused : in Boolean);

  package Fifo is new Channels.Channel ("test_tcp", Message_Type, Fifo_Cb);

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
    if not Publish then
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
      exit when not Publish and then Nb_Done = Nb_To_Do;
    end loop;
  end Wait;

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
  Nb_To_Do := Positive'Value (Argument.Get_Parameter(2));
  Nb_Done := 0;

  if not Publish then
    -- Subscriber
    Ada.Text_Io.Put_Line ("Subscribing");
    Fifo.Subscribe;
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
  Ada.Text_Io.Put_Line ("Adding dests localhost and penelope");
  Fifo.Add_Destination ("localhost");
  Fifo.Add_Destination ("penelope");
  Wait (1.0);
  
  Ada.Text_Io.Put_Line ("Sending messages");
  loop
    Fifo.Write (Message);
    Nb_Done := Nb_Done + 1;
    -- Wait for replies
    Wait (0.5);
    exit when Nb_Done = Nb_To_Do;
  end loop;

  Ada.Text_Io.Put_Line ("Deleting dest penelope and sending two messages");
  Fifo.Del_Destination ("penelope");
  Fifo.Write (Message);
  Fifo.Write (Message);
  Wait (0.5);


  Ada.Text_Io.Put_Line ("Deleting all dests and sending one message");
  Fifo.Del_All_Destinations;
  Fifo.Write (Message);
  Wait (0.5);

  Ada.Text_Io.Put_Line ("Sending one message");
  Fifo.Write (Message);
  Wait (0.5);

end T_Channels;

