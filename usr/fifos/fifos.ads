-- Point to point connection
-- Several senders can connect to a reception Fifo
-- Once connected, a Fifo is full duplex
-- A Dictio must be up and running for Open to succeed
with Socket, Tcp_Util, Dynamic_List;
package Fifos is

  -- Max length of a Fifo name
  Max_Fifo_Name_Len : constant := 32;

  -- Length of a message
  subtype Message_Length is Natural;

  -- Fifo kind:
  --  Connect (open to remote),
  --  Accepting (open not to remote),
  --  Accepted (by an accepting fifo)
  type Fifo_Kind_List is (Connect, Accepting, Accepted);

  -- Fifo state:
  --  Waiting (for a dictio update after connection failure),
  --  Connecting (trying to connect 3 tries of 3 seconds),
  --  Connected (connected to remote or accepting or accepted)
  type Fifo_State_List is (Waiting, Connecting, Connected);

  -- Result of message sending
  type Send_Result_List is (Ok, Overflow, Disconnected, Error);

  -- Exceptions
  Name_Too_Long, Yet_Open, Not_Open, In_Overflow,
              Name_Error, No_Dictio, System_Error : exception;

  generic
    type Message_Type is private;
  package Fifo is

    -- Fifo unique identifier
    type Fifo_Id is private;
    No_Fifo : constant Fifo_Id;

    -- Invoqued when a "connect" fifo is connected/disconnected to remote,
    --  or when an accepting fifo accepts a connection (a new accepted fifo is
    --   created),
    --  or when an accepted fifo becomes disconnected from remote (this fifo
    --   becomes closed).
    type Connection_Callback_Access is
         access procedure (Fifo_Name : in String;
                           Id        : in Fifo_Id;
                           Connected : in Boolean);

    -- Invoqued when receiving a message
    type Reception_Callback_Access is 
         access procedure (Id      : in Fifo_Id;
                           Message : in Message_Type;
                           Length  : in Message_Length);

    -- Invoqued when end of overflow after sending overflow
    type End_Overflow_Callback_Access is
       access procedure (Id : in Fifo_Id);

    -- Open a connection to remote or accept connections from remotes
    -- May raise Name_Too_Long if Fifo_Name is longer than Max_Fifo_Name_Len
    -- May raise Yet_Open if Fifo_Name is already open (in same way)
    -- May raise Name_Error if Fifo_Name is not known in services (reception)
    -- May raise No_Dictio if dictio is unreachable
    -- May raise System_Error on various error conditions
    -- Resulting Fifo_Id is valid if successful open
    function Open (Fifo_Name       : in String;
                   To_Remote       : in Boolean;
                   Connection_Cb   : in Connection_Callback_Access;
                   Reception_Cb    : in Reception_Callback_Access;
                   End_Overflow_Cb : in End_Overflow_Callback_Access)
             return Fifo_Id;

    -- Close a connection to a remote
    --  or close a connection from a remote
    --  or stop accepting connections from remotes (which remain connected)
    -- May raise Not_Open if Fifo is not open
    procedure Close (Id : in out Fifo_Id);

    -- Close all fifos of all kind this Message_Type
    procedure Close_All;

    -- Kind and state of a fifo
    -- May raise Not_Open if Fifo is not open
    function Fifo_Kind (Id : Fifo_Id) return Fifo_Kind_List;
    function Fifo_State (Id : Fifo_Id) return Fifo_State_List;

    -- Activate or not the reception of messages
    --  from a Connect, Accepting (for all accepted) 
    --  or accepted fifo
    -- Note that only reception is concerned: desactivating
    --  a Connecting fifo does not prevent it from accepting, but
    --  makes that Accepted fifos will be desactivated
    -- May raise Not_Open if Fifo is not open
    procedure Activate (Id              : in Fifo_Id;
                        Allow_Reception : in Boolean);

    -- Check if reception is active on a fifo
    -- May raise Not_Open if Fifo is not open
    function Is_Active (Id : Fifo_Id) return Boolean;

    -- Send a message
    -- May raise Not_Open if Fifo is not open
    -- May raise In_Overflow if previous Send returned Overflow and
    --  End_Overflow_Cb has not (yet) be called
    function Send (Id      : in Fifo_Id;
                   Message : in Message_Type;
                   Length  : in Message_Length := 0) return Send_Result_List;

  private
    -- Record for connecting/connected accepting or accepted fifo
    type Fifo_Rec is record
      Kind : Fifo_Kind_List;
      Name : String (1 .. Max_Fifo_Name_Len);
      Len  : Positive;
      Dscr  : Socket.Socket_Dscr;
      Afux_Dscr :  Socket.Socket_Dscr; -- Afux_Socket for Accepting.
      State : Fifo_State_List;
      Active : Boolean;
      Host : Tcp_Util.Remote_Host;
      Port : Tcp_Util.Remote_Port;
      Conn_Cb : Connection_Callback_Access;
      Rece_Cb : Reception_Callback_Access;
      Ovfl_Cb : End_Overflow_Callback_Access;
    end record;
    package Fifo_Dyn_List_Mng is new Dynamic_List (Fifo_Rec);
    package Fifo_List_Mng renames Fifo_Dyn_List_Mng.Dyn_List;
    subtype Fifo_Access is Fifo_List_Mng.Element_Access;

    type Fifo_Id is record
      Acc : Fifo_Access := null;
    end record;
    No_Fifo : constant Fifo_Id := (Acc => null);

  end Fifo;
 
end Fifos;

