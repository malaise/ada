package Channels is

  -- Lenght of message exchanged on channel
  subtype Message_Length is Natural;

  -- Callback invoqued in Write after each transmission to a destination
  type Send_Callback_Access is access
    procedure (Host_Name : in String;
               Send_Ok   : in Boolean);

  -- Exceptions raised in a channel
  -- Calling Change_Channel_Name while Subscribed or a Destination is set
  Channel_Active : exception;

  -- Channel or Destination host name too long (see Tcp_Util)
  Name_Too_Long : exception;

  -- Channel_Name not set in services
  Unknown_Channel : exception;
  -- Host_Name not set in hosts or not previously added
  Unknown_Destination : exception;

  -- Subscribing twice to the same channel
  Already_Subscribed : exception;
  -- Unsubscribing from a channel no subscribed to
  Not_Subscribed : exception;

  -- Adding twice the same destination to a channel
  Destination_Already : exception;

  -- Replying while not in Read_Cb
  Not_In_Read : exception;
  -- Reply sending has failed
  Reply_Failed : exception;

  -- File not found or syntax error in add_destinations
  File_Error : exception;

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
    procedure Add_Destinations (File_Name : in String);

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
    -- File_Error may be raised if file cannot be open, read or incorrect syntax detected


    -- Delete a recipient
    -- May raise Unknown_Destination if Host_Name has not been added
    procedure Del_Destination (Host_Name : in String);

    -- Delete all recipients
    procedure Del_All_Destinations;


    -- Send a message to all recipients
    -- Callback is invoqued for each recipient with the result of sending
    procedure Write (Message : in Message_Type;
                     Length  : in Message_Length := 0;
                     Send_Cb : in Send_Callback_Access := null);

    -- Reply to sender of last message received
    -- Should only be called in Read_Cb.
    -- May raise Not_In_Read if not called by Read_Cb
    -- May raise Reply_Failed if reply cannot be sent
    procedure Reply (Message : in Message_Type;
                     Length : in Message_Length := 0);

  end Channel;

end Channels;

