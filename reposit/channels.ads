package Channels is

  -----------------
  -- CONVENTIONS --
  -----------------
  -- Some services rely on Even_Mng-Fd callbacks that return true at least when
  --  Read_Cb is called (other internal events, see Tcp_Util may lead to
  --  positive Fd_Event reports

  -----------------
  -- DEFINITIONS --
  -----------------
  -- Lenght of message exchanged on channel
  subtype Message_Length is Natural;

  -- Callback invoqued in Channel.Write after each transmission to a
  --  destination
  type Send_Callback_Access is access
    procedure (Host_Name : in String;
               Send_Ok   : in Boolean);

  -- Exceptions raised in a channel or bus
  ----------------------------------------
  -- Channel, Destination, Bus or Lan name too long (see Tcp_Util)
  Name_Too_Long : exception;
  -- Replying while not in Read_Cb
  Not_In_Read : exception;

  -- Host_Name not set in hosts or not previously added (channel)
  -- Lan_Name not set in networks (bus)
  Unknown_Destination : exception;

  -- Subscribing twice to the same channel or bus
  Already_Subscribed : exception;
  -- Unsubscribing from a channel or bus not subscribed to
  Not_Subscribed : exception;

  -- Reply sending has failed
  Reply_Failed : exception;
  -- Message sending has failed
  Send_Failed : exception;

  -- Exceptions raised in a channel
  ---------------------------------
  -- Calling Change_Channel_Name while Subscribed or a Destination is set
  Channel_Active : exception;

  -- Channel_Name not set in services (tcp)
  Unknown_Channel : exception;

  -- Adding twice the same destination to a channel
  Destination_Already : exception;

  -- Reply sending has failed due to overflow
  Reply_Overflow : exception;

  -- Message sending has failed due to overflow
  Send_Overflow : exception;

  -- File not found or syntax error in Add_destinations
  File_Error : exception;

  -- Exceptions raised in a bus
  -----------------------------
  -- Calling Change_Bus_Names while subscribed or a joined
  Bus_Active : exception;

  -- Bus name not set in services (udp)
  Unknown_Bus : exception;

  -- Joining twice to the same bus
  Already_Joined : exception;
  -- Writing without joining
  Not_Joined : exception;


  -------------
  -- CHANNEL --
  -------------
  generic
    -- Name of the channel (tcp in services)
    Channel_Name : in String;
    -- Type of message exchanged on the channel
    type Message_Type is private;
    -- Callback invoqued to receive a message on the channel
    with procedure Read_Cb (Message  : in Message_Type;
                            Length   : in Message_Length;
                            Diffused : in Boolean);

  package Channel is

    -- The message kind sent on socket (Tcp header)
    type Channel_Message_Type is record
      Diff : Boolean := True;
      Data : Message_Type;
    end record;

    -- Change channel name
    -- May raise Name_Too_Long if Channel_Name is too long
    -- May raise Channel_Active if Subscribed or a Destination is set
    procedure Change_Channel_Name (New_Channel_Name : in String);


    -- Subscription
    -- Allow connections from remote processes to local channel
    -- May raise Already_Subscribed if already subscribed to this channel
    -- May raise Name_Too_Long if Channel_Name is too long
    -- May raise Unknown_Channel if Channel_Name is not known
    procedure Subscribe;

    -- Close all connections from remote process and forbid new connections
    -- May raise Not_Subscribed if not subscribed to this channel
    procedure Unsubscribe;


    -- Add a new destination for messages sent on the channel
    -- May raise Destination_Already if this destination has already be added
    -- May raise Unknown_Channel if Channel_Name is not known
    -- May raise Unknown_Destination if Host_Name is not known
    procedure Add_Destination (Host_Name : in String);

    -- Add new destinations from a Ascii file
    -- File format is:
    -- <channel_list> ::= [ { <channel_declaration> } ]
    -- <channel_declaration> ::= Channel { <channel_name> }
    --                             [ { <host_declaration> } ]
    --                           End_Channel
    -- host_declaration ::= Host [ { <host_name> } ]

    -- Example:
    -- Channel test_tcp other
    --   Host portillon ulysse
    --   Host penelope
    -- End_Channel

    -- Notes:
    -- # at the beginning of a line is comment
    -- Line must be empty, or a comment, or start by either Channel, Host
    --   or End_Channel
    -- The same channel_name may appear several times in the file
    -- Destination_Already is not raised if a host is already added
    --  (by Add_Destination or within the same or another channel declaration
    --  in the file
    -- Unknown_Channel may be raised if Channel_Name is not known
    -- Unknown_Destination is not raised if host_name is not known
    -- File_Error may be raised if file cannot be open, read or incorrect
    --  syntax detected
    procedure Add_Destinations (File_Name : in String);


    -- Delete a recipient
    -- May raise Unknown_Destination if Host_Name has not been added
    procedure Del_Destination (Host_Name : in String);

    -- Delete all recipients
    procedure Del_All_Destinations;


    -- Activate or not the reception of messages
    -- on the channel (activated by default)
    procedure Activate (Allow_Reception : in Boolean);

    -- Is reception active
    function Is_Active return Boolean;

    -- Send a message to all recipients
    -- Callback is invoqued for each recipient with the result of sending
    procedure Write (Message : in Message_Type;
                     Length  : in Message_Length := 0;
                     Send_Cb : access
      procedure (Host_Name : in String;
                 Send_Ok   : in Boolean) := null);

    -- Reply to sender of last message received
    -- Should only be called in Read_Cb.
    -- May raise Not_In_Read if not called by Read_Cb
    -- May raise Reply_Overflow if reply cannot be sent due to overflow
    -- May raise Reply_Failed if reply cannot be sent due to other error
    procedure Reply (Message : in Message_Type;
                     Length : in Message_Length := 0);

    -- Send a message to one destination
    -- May raise Unknown_Destination if Host_Name is not known
    -- May raise Send_Overflow if message cannot be sent due to overflow
    -- May raise Send_Failed if message cannot be sent due to other error
    procedure Send (Host_Name : in String;
                    Message   : in Message_Type;
                    Length    : in Message_Length := 0);

  end Channel;


  ---------
  -- BUS --
  ---------
  generic
    -- Name of the bus (udp in services)
    Bus_Name : in String;

    -- Name of the Lan (in setworks)
    Destination_Name : in String;

    -- Type of message exchanged on the channel
    type Message_Type is private;
    -- Callback invoqued to receive a message on the channel
    with procedure Read_Cb (Message  : in Message_Type;
                            Length   : in Message_Length;
                            Diffused : in Boolean);

  package Bus is

    -- The message kind sent on socket (Udp/Ipm)
    type Bus_Message_Type is record
      Diff : Boolean := True;
      Data : Message_Type;
    end record;

    -- Change bus and Lan names
    -- May raise Name_Too_Long if a Name is too long
    -- May raise Bus_Active if subscribed or joined
    procedure Change_Names (New_Bus_Name, New_Destination_Name : in String);


    -- Subscription
    -- Allow reception from bus
    -- May raise Already_Subscribed if already subscribed to this bus
    -- May raise Name_Too_Long if Bus or Destination Name is too long
    -- May raise Unknown_Bus if Bus_Name is not known
    -- May raise Unknown_Destination if Destination_Name is not known
    procedure Subscribe;

    -- Close reception from bus
    -- May raise Not_Subscribed if not subscribed to this bus
    procedure Unsubscribe;


    -- Join a bus for publishing
    -- May raise Already_Joined if already joined
    -- May raise Unknown_Bus if Bus_Name is not known
    -- May raise Unknown_Destination if Destination_Name is not known
    procedure Join;

    -- Leave a bus
    -- May raise Not_Joined if not joined
    procedure Leave;

    -- Send a message on the bus
    -- May raise Not_Joined if not joined
    procedure Write (Message : in Message_Type;
                     Length  : in Message_Length := 0);

    -- Reply to sender of last message received
    -- Should only be called in Read_Cb.
    -- May raise Not_Subscribed if not subscribed (any more) to this bus
    -- May raise Not_In_Read if not called by Read_Cb
    -- May raise Reply_Failed if reply cannot be sent
    procedure Reply (Message : in Message_Type;
                     Length : in Message_Length := 0);

    -- Send a message to one destination
    -- May raise Not_Joined if not joined
    -- May raise Unknown_Destination if Host_Name is not known
    -- May raise Send_Failed if message cannot be sent
    procedure Send (Host_Name : in String;
                    Message   : in Message_Type;
                    Length    : in Message_Length := 0);

  end Bus;

end Channels;

