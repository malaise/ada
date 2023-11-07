package body Socket_Util is

  -- Set destination host/lan name or Ip address, and port name or num
  -- Lan is significant only of Host.Kind is Host_Name_Spec
  procedure Set_Destination (Soc  : in Socket_Dscr;
                             Lan  : in Boolean;
                             Host : in Remote_Host;
                             Port : in Remote_Port) is
  begin
    case Host.Kind is
      when Host_Name_Spec =>
        case Port.Kind is
          when Port_Name_Spec =>
            Soc.Set_Destination_Name_And_Service (Lan,
                  Host.Name.Image, Port.Name.Image);
          when Port_Num_Spec =>
            Soc.Set_Destination_Name_And_Port (Lan,
                  Host.Name.Image, Port.Num);
        end case;
      when Host_Id_Spec =>
        case Port.Kind is
          when Port_Name_Spec =>
            Soc.Set_Destination_Host_And_Service (Host.Id, Port.Name.Image);
          when Port_Num_Spec =>
            Soc.Set_Destination_Host_And_Port (Host.Id, Port.Num);
        end case;
    end case;
  end Set_Destination;

  -- Change destination host/lan name or Ip address
  -- Lan is significant only of Host.Kind is Host_Name_Spec
  procedure Change_Destination (Soc  : in Socket_Dscr;
                                Lan  : in Boolean;
                                Host : in Remote_Host) is
  begin
    case Host.Kind is
      when Host_Name_Spec =>
        Soc.Change_Destination_Name (Lan, Host.Name.Image);
      when Host_Id_Spec =>
        Soc.Change_Destination_Host (Host.Id);
    end case;
  end Change_Destination;

  -- Change destination port name or num
  procedure Change_Destination (Soc  : in Socket_Dscr;
                                Port : in Remote_Port) is
  begin
    case Port.Kind is
      when Port_Name_Spec =>
        Soc.Change_Destination_Service (Port.Name.Image);
      when Port_Num_Spec =>
        Soc.Change_Destination_Port (Port.Num);
    end case;
  end Change_Destination;

  -- Link to a port name or num
  procedure Link (Soc  : in Socket_Dscr;
                  Port : in Local_Port) is
  begin
    case Port.Kind is
      when Port_Name_Spec =>
        Soc.Link_Service (Port.Name.Image);
      when Port_Num_Spec =>
        Soc.Link_Port (Port.Num);
      when Port_Dynamic_Spec =>
        Soc.Link_Dynamic;
    end case;
  end Link;

  procedure Link (Soc  : in Socket_Dscr;
                  Port : in Remote_Port) is
  begin
    case Port.Kind is
      when Port_Name_Spec =>
        Soc.Link_Service (Port.Name.Image);
      when Port_Num_Spec =>
        Soc.Link_Port (Port.Num);
    end case;
  end Link;

end Socket_Util;

