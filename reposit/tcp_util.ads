-- Automatically connect, re-connect, accept, handle sending overflow and
--  receive data on a TCP socket
-- Also send and receive on a UDP/IPM socket
with Socket, Socket_Util;
package Tcp_Util is

  -- GENERAL CONVENTIONS --
  -------------------------
  -- All services rely on Even_Mng-Fd and/or Timer callbacks, which return
  --  True at completion of the service. So Event_Mng will report:
  -- For Connection: Fd_Event or Timer_Event when connection success or failure
  -- For Acception: Fd_Event when a connection is accepted
  -- For Sending : Fd_Event when end of overflow or error
  -- For Receiving: Fd_Event when data read and callback returns true
  --                or when connection closed


  -- PROTOCOL DEFINITION --
  -------------------------
  -- All kinds of TCP of Socket are supported by Connect_To and Accept_From
  subtype Tcp_Protocol_List is Socket.Tcp_Protocol_List;
  -- Default TTL
  Default_Ttl : constant Socket.Ttl_Range := 0;

  -- POSITIVE and NATURAL DURATION
  --------------------------------
  -- Positive duration for connection
  subtype Positive_Duration is Duration range 0.001 .. Duration'Last;
  -- Natural duration for sending
  subtype Natural_Duration is Duration range 0.0 .. Duration'Last;

  -- CONNECTION PROCEDURES --
  ---------------------------
  -- Connection / Disconnection callback
  -- Sets Connected to True if connection succeeds,
  --  then Dscr is the one of the new socket connected, in mode Blocking_Send,
  --  Remote_Host_Id and Remote_Port_Num are set.
  -- Sets Connected to False if connection fails,
  --  then Dscr is Socket.No_Socket, Remote_Host_Id and Remote_Port_Num
  --  are set.
  type Connection_Callback_Access is
    access procedure (Remote_Host_Id  : in Socket_Util.Host_Id;
                      Remote_Port_Num : in Socket_Util.Port_Num;
                      Connected       : in Boolean;
                      Dscr            : in Socket.Socket_Dscr);

  -- Connect to a remote Host/Port
  -- May make several tries (one each Delta_Retry) before giving up
  -- Infinite retries if Nb_Tries = 0
  -- The Ttl is used (if supported by the TCP stack) to establish the
  --  connection, and later on to send on the established connection
  -- Returns True if immediate result could be achieved
  --  (and the connection callback has already been called).
  -- May raise Name_Error if Host.Name or Port.Name is unknown
  function Connect_To (Protocol      : in Tcp_Protocol_List;
                       Host          : in Socket_Util.Remote_Host;
                       Port          : in Socket_Util.Remote_Port;
                       Connection_Cb : in Connection_Callback_Access;
                       Delta_Retry   : in Positive_Duration := 1.0;
                       Nb_Tries      : in Natural := 1;
                       Ttl           : in Socket.Ttl_Range := Default_Ttl)
           return Boolean;

  -- Abort a pending connection attempt
  -- May raise No_Such
  procedure Abort_Connect (Host : in Socket_Util.Remote_Host;
                           Port : in Socket_Util.Remote_Port);

  -- Synchronously connect to a remote Host/Port
  -- The Ttl is used (if supported by the TCP stack) to establish the
  --  connection, and later on to send on the established connection
  -- Timeout = 0.0 may be used for infinite attempt
  -- Returns a valid Dscr (Open, Full blocking) if success, otherwise
  --  Socket.No_Socket
  -- May raise Name_Error if Host.Name or Port.Name is unknown
  function Connect_To (Protocol      : in Tcp_Protocol_List;
                       Host          : in Socket_Util.Remote_Host;
                       Port          : in Socket_Util.Remote_Port;
                       Timeout       : in Natural_Duration := 1.0;
                       Ttl           : in Socket.Ttl_Range := Default_Ttl)
           return Socket.Socket_Dscr;

  -- ACCEPTION PROCEDURE --
  -------------------------
  -- Acception callback
  -- The Local_Dscr is the one set by Accept_From
  -- New_Dscr is the one of the new socket, in mode Blocking_Send,
  -- Remote_Host_Id and Remote_Port_Num are set.
  type Acception_Callback_Access is
    access procedure (Local_Port_Num  : in Socket_Util.Port_Num;
                      Local_Dscr      : in Socket.Socket_Dscr;
                      Remote_Host_Id  : in Socket_Util.Host_Id;
                      Remote_Port_Num : in Socket_Util.Port_Num;
                      New_Dscr        : in Socket.Socket_Dscr);

  -- Accept connections to a local port, possibly on a specific interface
  -- Dscr is open in mode Blocking_Send, and set to the accept connections
  -- Num is its port num (usefull when dynamic).
  -- Link_If can be set in order to accept on a specific network interface
  -- May raise Name_Error if Port.Name is unknown
  procedure Accept_From (Protocol     : in Tcp_Protocol_List;
                         Port         : in Socket_Util.Local_Port;
                         Acception_Cb : in Acception_Callback_Access;
                         Dscr         : out Socket.Socket_Dscr;
                         Num          : out Socket_Util.Port_Num;
                         Link_If      : in Socket.Host_Id := Socket.Any_Host);

  -- Abort further accepts on port (Protocol is necessary because Af_Inet and
  --  Af_Unix may be accepting in paralllel on the same port)
  --  and closes the Dscr
  -- May raise No_Such
  procedure Abort_Accept (Protocol : in Tcp_Protocol_List;
                          Num      : in Socket_Util.Port_Num);


  -- SEND PROCEDURES --
  ---------------------
  -- End of overflow callback
  type End_Overflow_Callback_Access is
    access procedure (Dscr : in Socket.Socket_Dscr);

  -- Callback invoked on fatal error during sending (or re-sending):
  --  on connection lost or timeout
  -- Dscr is closed after invocation
  type Send_Error_Callack_Access is
    access procedure (Dscr : in Socket.Socket_Dscr;
                      Conn_Lost : in Boolean);

  -- The general idea is not to be blocked in case the receiver is too slow
  --  or if the connection becomes "frozen" (TCP timeout is very long).
  -- There are two strategies:
  --  - either set the socket blocking and send with a timeout. The send
  --    blocks and either succeeds or raises Timeout_Error or
  --    Socket.Soc_Conn_Lost
  --  - or set the socket non blocking. The send either succeds or raises
  --    Socket.Conn_Lost or returns False.
  --    It tries asynchronously to re-send when possible until all the message
  --    is sent, then calls End_Of_Overflow_Callback.
  --    If the timeout occurs or connection is lost during the retries,
  --    then it calls the Send_Error_Callback and closes the Dscr.
  -- May raise Soc_Dest_Err if destination is not set
  -- May raise Soc_Conn_Err if tcp and not connected
  -- May raise Socket.Soc_Tail_Err if called while previous Send returned
  --  False and End_Of_Overflow_Cb has not (yet) been called
  -- May raise Socket.Soc_Conn_Lost if the connection is lost, the socket
  --  should be closed
  -- May raise Timeout_Error on a blocking socket if timeout has expired, the
  --  socket is left in an unpredictable state and MUST be closed.
  -- Notes:
  -- If the timeout is 0.0 then no timeout is checked and Timeout_Error cannot
  --  be raised: sending on a blocking socket blocks until success or error,
  --  and sending on an non blocking socket makes infinite retries until
  --  success or error.
  -- If the socket is blocking then the callbacks are not used.
  -- If send is called on a non blocking socket and overflows and then the
  --  socket is changed to blocking then a Timeout error is reported as
  --  soon as possible (Timeout expiration or next attempt to re-send).
  -- This function can be used on a UDP/IPM socket but then it adds no value
  --  compared to Socket.Send
  Timeout_Error : exception;
  generic
    type Message_Type (<>) is private;
  function Send (Dscr               : in Socket.Socket_Dscr;
                 End_Of_Overflow_Cb : in End_Overflow_Callback_Access;
                 Send_Error_Cb      : in Send_Error_Callack_Access;
                 Timeout            : in Natural_Duration;
                 Message            : in Message_Type;
                 Length             : in Natural := 0) return Boolean;

  -- If a socket in overflow (send has returned False and End_Of_Overflow_Cb
  --  has not be called yet) has to be closed, then Abort_Send_and_Close
  --  has to be called instead of Socket.Close
  -- The socket is closed.
  -- May raise No_Such
  procedure Abort_Send_And_Close (Dscr : in out Socket.Socket_Dscr);


  -- RECEIVE PROCEDURE --
  -----------------------
  -- This package can be used with UDP/IPM socket as well (there is no
  --  disconnection in this case)

  -- Callback invoqued when remote disconnects
  -- Callbacks are automatically removed
  --  and Dscr is closed after invocation
  type Disconnection_Callback_Access is access
       procedure (Dscr : in Socket.Socket_Dscr);

  generic
    type Message_Type is private;
  package Reception is
    -- Callback invoqued when a message is received
    -- If it returns True then a Fd_Event will be reported
    --  by Event_Mng
    type Reception_Callback_Access is access
         function (Dscr    : Socket.Socket_Dscr;
                   Message : Message_Type;
                   Length  : Natural) return Boolean;

    -- Set reception and disconnection callbacks
    --  (callbacks may be null, then data/events will be lost)
    -- Callbacks are activated
    -- Set_For_Reply may be set for each read
    -- May raise No_Such if Dscr is not open
    procedure Set_Callbacks (
                    Dscr             : in Socket.Socket_Dscr;
                    Reception_Cb     : in Reception_Callback_Access;
                    Disconnection_Cb : in Disconnection_Callback_Access;
                    Set_For_Reply    : in Boolean := False);

    -- Activate or freeze the reception on connection
    -- When a connection is frozen, data/events are kept
    --  on the connection (tcp buffers and flow control)
    -- May raise No_Such if callbacks have not been set
    procedure Activate_Callbacks (Dscr   : in Socket.Socket_Dscr;
                                  Active : in Boolean);

    -- Are current callbacks active
    -- May raise No_Such if callbacks have not been set
    function Callbacks_Active (Dscr : Socket.Socket_Dscr) return Boolean;

    -- Remove the callbacks
    -- To be called before closing a descriptor on which callbacks
    --  have been set
    -- May raise No_Such if callbacks have not been set
    procedure Remove_Callbacks (Dscr : in Socket.Socket_Dscr);

    -- Are callbacks set
    function Callbacks_Set (Dscr : Socket.Socket_Dscr) return Boolean;

  end Reception;

  -- EXCEPTIONS --
  ----------------
  -- Raised by Connect_To if Host.Name or Port.Name is unknown
  Name_Error : exception;

  -- Raised when aborting unknown connection/acception
  -- Or a send which is not in overflow
  -- Or setting reception callback on not open Dscr
  No_Such : exception;

end Tcp_Util;

