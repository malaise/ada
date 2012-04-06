-- Automatically connect, re-connect, accept, handle sending overflow and
--  receive data on a TCP socket
with As.U, Socket;
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
  -- All kinds of TCP of Socket are supported by Connect_To and Accepti_From
  subtype Tcp_Protocol_List is Socket.Protocol_List range
                               Socket.Tcp .. Socket.Tcp_Header_Afux;
  -- Default TTL
  Default_Ttl : constant Socket.Ttl_Range := 0;

  -- POSITIVE and NATURAL DURATION
  --------------------------------
  -- Positive duration for connection
  subtype Positive_Duration is Duration range 0.001 .. Duration'Last;
  -- Natural duration for sending
  subtype Natural_Duration is Duration range 0.0 .. Duration'Last;

  -- PORT DEFINITION --
  ---------------------
  -- Kinds of port definition
  type Local_Port_List is (Port_Name_Spec, Port_Num_Spec, Port_Dynamic_Spec);
  -- No dynamic remote port
  subtype Remote_Port_List is Local_Port_List
                     range Port_Name_Spec .. Port_Num_Spec;

  -- Port name and num
  subtype Port_Name is As.U.Asu_Us;
  subtype Port_Num is Socket.Port_Num;

  -- Specification of a local port (name or num or dynamic) to bind to
  type Local_Port (Kind : Local_Port_List := Port_Name_Spec) is record
    case Kind is
      when Port_Name_Spec =>
        Name : Port_Name := As.U.Asu_Null;
      when Port_Num_Spec =>
        Num : Port_Num := 0;
      when Port_Dynamic_Spec =>
        null;
    end case;
  end record;

  -- Specification of a remote port (name or num) to connect or send to
  type Remote_Port (Kind : Remote_Port_List := Port_Name_Spec) is record
    case Kind is
      when Port_Name_Spec =>
        Name : Port_Name := As.U.Asu_Null;
      when Port_Num_Spec =>
        Num : Port_Num := 0;
    end case;
  end record;

  -- HOST DEFINITION --
  ---------------------
  -- Kinds of host definition
  type Remote_Host_List is (Host_Name_Spec, Host_Id_Spec);
  -- Host name and id
  subtype Host_Name is As.U.Asu_Us;
  subtype Host_Id is Socket.Host_Id;

  -- Specification of a remote host to connect or send to
  type Remote_Host (Kind : Remote_Host_List := Host_Name_Spec) is record
    case Kind is
      when Host_Name_Spec =>
        Name : Host_Name := As.U.Asu_Null;
      when Host_Id_Spec =>
        Id : Host_Id := Socket.No_Host;
    end case;
  end record;

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
    access procedure (Remote_Host_Id  : in Host_Id;
                      Remote_Port_Num : in Port_Num;
                      Connected       : in Boolean;
                      Dscr            : in Socket.Socket_Dscr);

  -- Connect to a remote Host/Port
  -- May make several tries (one each Delta_Retry) before giving up
  -- Infinite retries if Nb_Tries = 0
  -- The Ttl is used (if supported by the TCP stack) to establish the
  --  connection and in the established connection
  -- Returns True if immediate result could be achieved
  --  (and the callback has already been called).
  -- May raise Name_Error if Host.Name or Port.Name is unknown
  function Connect_To (Protocol      : in Tcp_Protocol_List;
                       Host          : in Remote_Host;
                       Port          : in Remote_Port;
                       Connection_Cb : in Connection_Callback_Access;
                       Delta_Retry   : in Positive_Duration := 1.0;
                       Nb_Tries      : in Natural := 1;
                       Ttl           : in Socket.Ttl_Range := Default_Ttl)
           return Boolean;

  -- Abort a pending connection attempt
  -- May raise No_Such
  procedure Abort_Connect (Host : in Remote_Host;
                           Port : in Remote_Port);

  -- Synchronously connect to a remote Host/Port
  -- The Ttl is used (if supported by the TCP stack) to establish the
  --  connection and in the established connection
  -- Timeout = 0.0 may be used for infinite attempt
  -- Returns a valid Dscr (Open, Full blocking) if success
  -- May raise Name_Error if Host.Name or Port.Name is unknown
  function Connect_To (Protocol      : in Tcp_Protocol_List;
                       Host          : in Remote_Host;
                       Port          : in Remote_Port;
                       Timeout       : in Natural_Duration := 1.0;
                       Ttl           : in Socket.Ttl_Range := Default_Ttl)
           return Socket.Socket_Dscr;

  -- ACCEPTION PROCEDURE --
  -------------------------
  -- Acception callback
  -- The Local_Dscr is the one set by Accept_From
  --  New_Dscr is the one of the new socket, in mode Blocking_Send,
  --  Remote_Host_Id and Remote_Port_Num are set.
  type Acception_Callback_Access is
    access procedure (Local_Port_Num  : in Port_Num;
                      Local_Dscr      : in Socket.Socket_Dscr;
                      Remote_Host_Id  : in Host_Id;
                      Remote_Port_Num : in Port_Num;
                      New_Dscr        : in Socket.Socket_Dscr);

  -- Accept connections to a local port
  -- Dscr is open in mode Blocking_Send, and set to the accept connections
  -- Num is its port num (usefull when dynamical).
  -- May raise Name_Error if Port.Name is unknown
  procedure Accept_From (Protocol     : in Tcp_Protocol_List;
                         Port         : in Local_Port;
                         Acception_Cb : in Acception_Callback_Access;
                         Dscr         : out Socket.Socket_Dscr;
                         Num          : out Port_Num);

  -- Abort further accepts on port (Af_inet and Af_unix may be on the same port).
  --  and closes the Dscr
  -- May raise No_Such
  procedure Abort_Accept (Protocol : in Tcp_Protocol_List; Num : in Port_Num);


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
  --    If a timeout occurs or connection is lost during the retries,
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
  --   socket is changed to blocking then a Timeout error is reported as
  --   soon as possible (Timeout expiration or next attempt to re send).
  -- This function can be used on a UDP/IPM socket but adds no value compared
  --  to Socket.Send
  Timeout_Error : exception;
  generic
    type Message_Type is private;
  function Send (Dscr               : in Socket.Socket_Dscr;
                 End_Of_Overflow_Cb : in End_Overflow_Callback_Access;
                 Send_Error_Cb      : in Send_Error_Callack_Access;
                 Timeout            : in Natural_Duration;
                 Message            : in Message_Type;
                 Length             : in Natural := 0) return Boolean;

  -- If a socket in overflow (send has returned False and End_Of_Overflow_Cb
  --  has not be called yet) has to be closed, then Abort_Send_and_Close
  -- has to be called instead of Socket.Close
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
    -- May raise No_Such if Dscr is not open
    procedure Set_Callbacks (
                    Dscr             : in Socket.Socket_Dscr;
                    Reception_Cb     : in Reception_Callback_Access;
                    Disconnection_Cb : in Disconnection_Callback_Access);

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

