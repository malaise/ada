package Channels is

  subtype Message_Length is Natural;

  type Send_Callback_Access is access
    procedure (Host_Name : in String;
               Send_Ok   : in Boolean);

  generic
    -- The name of the channel (tcp in services)
    Channel_Name : in String;
    -- The type of message exchanged on the channel
    type Message_Type is private;
    -- The callback to receive messages
    with procedure Read_Cb (Message  : in Message_Type;
                            Length   : in Message_Length;
                            Diffused : in Boolean);
  package Channel is
    -- Channel or Destination host name too long
    Name_Too_Long : exception;

    -- Subscription
    -- Allow connections to local channel
    procedure Subscribe;
    Already_Subscribed : exception;

    -- Close all connections and forbid new connections
    procedure Unsubscribe;
    Not_Subscribed : exception;

    -- Add a new recipient
    procedure Add_Destination (Host_Name : in String);
    Destination_Already : exception;
    Unknown_Destination : exception;

    -- Delete a recipient
    procedure Del_Destination (Host_Name : in String);

    -- Delete all recipients
    procedure Del_All_Destinations;

    -- Send a message to all recipients
    procedure Write (Message : in Message_Type;
                     Length  : in Message_Length := 0;
                     Send_Cb : in Send_Callback_Access := null);

    -- Reply to sender of last message received
    -- Should only be called in Read_Cb.
    procedure Reply (Message : in Message_Type; Length : in Message_Length := 0);
    Not_In_Read, Reply_Failed : exception;

  end Channel;

end Channels;
  
