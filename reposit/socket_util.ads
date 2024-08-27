-- Use on Socket the Tcp_Util way to designate an address (destination or port)
with Socket, As.U;
package Socket_Util is
  subtype Socket_Dscr is Socket.Socket_Dscr;

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
  No_Local_Port : constant Local_Port := (others => <>);

  -- Specification of a remote port (name or num) to connect or send to
  type Remote_Port (Kind : Remote_Port_List := Port_Name_Spec) is record
    case Kind is
      when Port_Name_Spec =>
        Name : Port_Name := As.U.Asu_Null;
      when Port_Num_Spec =>
        Num : Port_Num := 0;
    end case;
  end record;
  No_Remote_Port : constant Remote_Port := (others => <>);

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
        Id : Host_Id := Socket.Any_Host;
    end case;
  end record;
  No_Host : constant Remote_Host := (others => <>);


  -- OPERATIONS
  -------------
  -- Set destination host/lan name or Ip address, and port name or num
  -- Lan is significant only of Host.Kind is Tcp_Util.Host_Name_Spec
  procedure Set_Destination (Soc  : in Socket_Dscr;
                             Lan  : in Boolean;
                             Host : in Remote_Host;
                             Port : in Remote_Port);

  -- Change destination host/lan name or Ip address
  -- Lan is significant only of Host.Kind is Tcp_Util.Host_Name_Spec
  procedure Change_Destination (Soc  : in Socket_Dscr;
                                Lan  : in Boolean;
                                Host : in Remote_Host);

  -- Change destination port name or num
  procedure Change_Destination (Soc  : in Socket_Dscr;
                                Port : in Remote_Port);

  -- Link to a port name or num
  procedure Link (Soc  : in Socket_Dscr;
                  Port : in Local_Port);
  procedure Link (Soc  : in Socket_Dscr;
                  Port : in Remote_Port);

end Socket_Util;

