with Socket;
package Tcp_Util is

  -- GENERAL CONVENTIONS --
  -------------------------
  -- Padd strings with spaces in records


  -- PROTOCOL DEFINITION --
  -------------------------
  subtype Tcp_Protocol_List is Socket.Protocol_List range
                               Socket.Tcp .. Socket.Tcp_Header;

  -- PORT DEFINITION --
  ---------------------
  -- Kinds of port definition
  type Local_Port_List is (Port_Name_Spec, Port_Num_Spec, Port_Dynamic_Spec);
  subtype Remote_Port_List is Local_Port_List
                     range Port_Name_Spec .. Port_Num_Spec;

  -- Port name and num
  Max_Port_Name_Len : constant := 50;
  subtype Port_Name is String (1 .. Max_Port_Name_Len);
  subtype Port_Num is Socket.Port_Num;


  -- Specification of a local port to bind to
  type Local_Port (Kind : Local_Port_List := Port_Name_Spec) is record
    case Kind is
      when Port_Name_Spec =>
        Name : Port_Name := (others => ' ');
      when Port_Num_Spec =>
        Num : Port_Num := 0;
      when Port_Dynamic_Spec =>
        null;
    end case;
  end record;


  -- Specification of a remote port to connect or send to
  type Remote_Port (Kind : Remote_Port_List := Port_Name_Spec) is record
    case Kind is
      when Port_Name_Spec =>
        Name : Port_Name := (others => ' ');
      when Port_Num_Spec =>
        Num : Port_Num := 0;
    end case;
  end record;


  -- HOST DEFINITION --
  ---------------------
  -- Kinds of host definition
  type Remote_Host_List is (Host_Name_Spec, Host_Id_Spec);
  -- Host name and id
  Max_Host_Name_Len : constant := 50;
  subtype Host_Name is String (1 .. Max_Host_Name_Len);
  subtype Host_Id is Socket.Host_Id;


  -- Specification of a remote host to connect or send to
  type Remote_Host (Kind : Remote_Host_List := Host_Name_Spec) is record
    case Kind is
      when Host_Name_Spec =>
        Name : Host_Name := (others => ' ');
      when Host_Id_Spec =>
        Id : Host_Id := Socket.No_Host;
    end case;
  end record;


  -- CALLBACKS --
  ---------------
  -- Connection / Disconnection callback
  -- Sets Connected to True if connection succeeds,
  --  then Dscr is the one of the new socket connected, blocking
  -- Sets Connected to False if connection fails,
  --  then Dscr is Socket.No_Socket,
  type Connection_Callback_Access is
    access procedure (Remote_Port_Num : in Port_Num;
                      Remote_Host_Id  : in Host_Id;
                      Connected       : in Boolean;
                      Dscr            : in Socket.Socket_Dscr);


  -- Acception callback
  -- The Local_Dscr is the one set by Accept_From
  -- Sets Connected to True if connection succeeds,
  --  then Dscr is the one of the new socket, blocking
  -- Sets Connected to False if connection breaks (and then closed),
  --  then Dscr is the one of the broken (and now closed) socket
  type Acception_Callback_Access is
    access procedure (Local_Port_Num  : in Port_Num;
                      Local_Dscr      : in Socket.Socket_Dscr;
                      Remote_Port_Num : in Port_Num;
                      Remote_Host_Id  : in Host_Id;
                      New_Dscr        : in Socket.Socket_Dscr);


  -- Default end of overflow callback
  type End_Overflow_Callback_Access is
    access procedure (Dscr : in Socket.Socket_Dscr);

  -- CONNECTION PROCEDURES --
  ---------------------------
  -- Connect to a remote Host/Port
  -- May make several tries (one each Delta_Retry) before giving up 
  -- Infinite retries if Nb_Tries = 0;
  -- Returns True if immediate result could be achieved
  --  (then callback has already been called)
  function Connect_To (Protocol      : in Tcp_Protocol_List;
                       Host          : in Remote_Host;
                       Port          : in Remote_Port;
                       Delta_Retry   : in Duration := 1.0;
                       Nb_Tries      : in Natural := 1;
                       Connection_Cb : in Connection_Callback_Access)
           return Boolean;

  -- Abort a pending connection
  -- May raise No_Such
  procedure Abort_Connect (Host : in Remote_Host;
                           Port : in Remote_Port);

  -- ACCEPTION PROCEDURE --
  -------------------------
  -- Accept connections to a local port
  -- Dscr is open and set to the accepting connections
  -- Num is its port num (usefull when dynamical)
  procedure Accept_From (Protocol     : in Tcp_Protocol_List;
                         Port         : in Local_Port;
                         Acception_Cb : in Acception_Callback_Access;
                         Dscr         : in out Socket.Socket_Dscr;
                         Num          : out Port_Num);

  -- Abort further accepts on port
  -- May raise No_Such
  procedure Abort_Accept (Num : in Port_Num);


  -- SEND PROCEDURES --
  ---------------------
  -- If send is ok returns True else
  -- Returns false and manages to re-send when possible until
  --  all message is sent, then calls End_Of_Overflow_Callback
  generic
    type Message_Type is private;
  function Send (Dscr               : in Socket.Socket_Dscr;
                 End_Of_Overflow_Cb : in End_Overflow_Callback_Access;
                 Message            : in Message_Type;
                 Length             : in Natural := 0) return Boolean;

  -- If a socket in overflow (send has returned True and End_Of_Overflow_CB
  --  has not be called yet) has to be closed, then Abort_Send_and_Close
  -- has to be called instead  of Socket.Close
  -- The socket is closed
  -- May raise No_Such
  procedure Abort_Send_and_Close (Dscr : in out Socket.Socket_Dscr);

  -- EXCEPTIONS --
  ----------------
  -- Raise when aborting unknown connection/acception
  -- Or a send which is not in overflow
  No_Such : exception;

end Tcp_Util;

