with Xml_Parser.Generator;
separate (Autobus)
package body Config is

  -- The ENV variable that contains the path of the XMl file
  Env_File_Name : constant String := "AUTOBUS_CONFIG";

  -- The Xml tree
  -- Parsed from the file and modified (host addresses i.o. names)
  -- Or initialized as Empty root (Autobus) if no file
  Ctx : Xml_Parser.Generator.Ctx_Type;

  -- Default values
  Default_Heartbeat_Period : constant Duration := 1.0;
  Default_Heartbeat_Max_Missed : constant := 3;
  Default_Timeout : constant Duration := 0.5;
  Default_Ttl : constant Socket.Ttl_Range := 5;

  -- For Debug
  function Image (Attrs : Xml_Parser.Attributes_Array) return String is
     Res : As.U.Asu_Us;
  begin
    for I in Attrs'Range loop
      Res.Append (Attrs(I).Name.Image & "=" & Attrs(I).Value.Image);
      if I /= Attrs'Last then
        Res.Append (" ");
      end if;
    end loop;
    return Res.Image;
  end Image;

  -- Each bus address is prefixed by "A-" to make it XML ID
  Bus_Prefix : constant String := "A-";
  function Bus_Id (Bus_Name : String) return String is
  begin
    return Bus_Prefix & Bus_Name;
  end Bus_Id;

  -- Get an attribute, raise Config_Error
  function Get_Attribute (Node : Xml_Parser.Element_Type;
                          Name : String;
                          Is_Duration : Boolean) return Duration is
    Attr : As.U.Asu_Us;
    Dur : Duration;
    Int : Integer;
  begin
    Attr := Ctx.Get_Attribute (Node, Name);
    if Is_Duration then
      Dur := Duration'Value (Attr.Image);
      if Dur <= 0.0 then
        raise Constraint_Error;
      end if;
      return Dur;
    else
      Int := Integer'Value (Attr.Image);
      if Int <= 0 then
        raise Constraint_Error;
      end if;
      return Duration (Int);
    end if;
  exception
    when Xml_Parser.Attribute_Not_Found =>
      return 0.0;
    when others =>
      Log_Error ("Config.Init", "invalid " & Name,
                 Attr.Image & " at line " &
                 Images.Integer_Image (Ctx.Get_Line_No (Node)));
      raise Config_Error;
  end Get_Attribute;

  -- Check the attributes of node
  Heartbeat_Period_Name : constant String := "Heartbeat_Period";
  Heartbeat_Max_Missed_Name : constant String := "Heartbeat_Max_Missed";
  Timeout_Name : constant String := "Timeout";
  Ttl_Name : constant String := "TTL";
  function Check_Attributes (Node : Xml_Parser.Element_Type) return Boolean is
    Dur : Duration;
    pragma Unreferenced (Dur);
  begin
    Dur := Get_Attribute (Node, Heartbeat_Period_Name, True);
    Dur := Get_Attribute (Node, Heartbeat_Max_Missed_Name, False);
    Dur:= Get_Attribute (Node, Timeout_Name, True);
    Dur:= Get_Attribute (Node, Ttl_Name, False);
    return True;
  exception
    when Config_Error =>
      return False;
  end Check_Attributes;

  -- The list of Bus nodes and corresponding Config
  type Bus_Conf_Rec is record
    Addr : As.U.Asu_Us;
    Bus : Xml_Parser.Node_Type;
  end record;
  function Bus_Conf_Match (Curr, Crit : Bus_Conf_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Curr.Addr = Crit.Addr;
  end Bus_Conf_Match;
  package Bus_Conf_Dyn_List_Mng is new Dynamic_List (Bus_Conf_Rec);
  package Bus_Conf_List_Mng renames Bus_Conf_Dyn_List_Mng.Dyn_List;
  Bus_Conf_List : Bus_Conf_List_Mng.List_Type;

  -- Check an IP address format
  function Check_Address (Str : in String) return Boolean is
    Host : Tcp_Util.Remote_Host;
    use type Tcp_Util.Remote_Host_List;
  begin
    Host := Ip_Addr.Parse (Str);
    if Host.Kind /= Tcp_Util.Host_Id_Spec then
      Log_Error ("Config.Init", "invalid alias address", Str);
      return False;
    end if;
    return True;
  end Check_Address;

  -- Check a bus name "A-<ip_addr>:<port>"
  function Check_Bus (Addr : String) return Boolean is
    Host : Tcp_Util.Remote_Host;
    Port : Tcp_Util.Remote_Port;
    use type Tcp_Util.Remote_Host_List, Tcp_Util.Remote_Port_List;
  begin
    if Addr'Length >= Bus_Prefix'Length
    and then Addr(Addr'First .. Addr'First+1) = Bus_Prefix then
      Ip_Addr.Parse (Addr(Addr'First+2 .. Addr'Last), Host, Port);
      if Host.Kind = Tcp_Util.Host_Id_Spec
      and then Port.Kind = Tcp_Util.Port_Num_Spec then
        return True;
      end if;
    end if;
    Log_Error ("Config.Init", "invalid bus name", Addr);
    return False;
  end Check_Bus;

  -- Init (parse the file)
  procedure Init is
    Root : Xml_Parser.Element_Type;
    Ok : Boolean;
    use type Xml_Parser.Ctx_Status_List;
  begin
    -- Check if already init
    if Ctx.Get_Status /= Xml_Parser.Clean then
      return;
    end if;

    -- Make dummy tree if env variable is set
    if not Environ.Is_Set (Env_File_Name)
    or else Environ.Getenv (Env_File_Name) = "" then
      Ctx.Set_Version (1, 0);
      Root := Ctx.Get_Root_Element;
      Ctx.Set_Name (Root, "Autobus");
      Ctx.Check (Ok);
      if not Ok then
        Log_Error ("Config.Init", Ctx.Get_Parse_Error_Message,
                   "on out dummy tree");
        raise Internal_Error;
      end if;
      Debug ("Dummy tree cause no Env");
      return;
    end if;

    -- Parse file, get Root
    Ctx.Parse (Environ.Getenv (Env_File_Name), Ok);
    if not Ok then
      Log_Error ("Config.Init", Ctx.Get_Parse_Error_Message,
                   "when parsing config file "
                 & Environ.Getenv (Env_File_Name));
        raise Config_Error;
    end if;
    Debug ("File " & Environ.Getenv (Env_File_Name) & " parsed OK");
    Root := Ctx.Get_Root_Element;

    -- Check all Buses, check that names are A-<Ip_Address>
    declare
      Buses : constant Xml_Parser.Nodes_Array := Ctx.Get_Children(Root);
      Name : As.U.Asu_Us;
      Node : Xml_Parser.Element_Type;
    begin
      Ok := True;
      if Check_Attributes (Root) then
        Debug ("Got default config " & Image (Ctx.Get_Attributes (Root)));
      else
        Ok := False;
      end if;

      for Bus in Buses'Range loop
        -- Get bus config
        Name := Ctx.Get_Attribute (Buses(Bus), 1).Value;
        if Check_Bus (Name.Image) then
          Debug ("Got bus " & Name.Image);
          if Check_Attributes (Buses(Bus)) then
            -- Store config for this bus
            Bus_Conf_List.Insert ( (Name, Buses(Bus)) );
            Debug ("  Got config "
                 & Image (Ctx.Get_Attributes (Buses(Bus))));
          else
            Ok := False;
          end if;
        else
          Ok := False;
        end if;

        for I in 1 .. Ctx.Get_Nb_Children (Buses(Bus)) loop
          Node := Ctx.Get_Child (Buses(Bus), I);
          if Ctx.Get_Name (Node) = "LANs" then
            -- Check LAN definitions
            declare
              Lans : constant Xml_Parser.Nodes_Array
                   := Ctx.Get_Children(Node);
            begin
              for Lan in Lans'Range loop
                if not Check_Address (Ctx.Get_Attribute (Lans(Lan),
                                                         "Address")) then
                  Ok := False;
                end if;
                if not Check_Address (Ctx.Get_Attribute (Lans(Lan),
                                                         "Netmask")) then
                  Ok := False;
                end if;
              end loop;
            end;
          elsif Ctx.Get_Name (Node) = "Aliases" then
            -- Check Alias definitions
            declare
              Aliases : constant Xml_Parser.Nodes_Array
                      := Ctx.Get_Children(Node);
            begin
              for Alias in Aliases'Range loop
                if not Check_Address (Ctx.Get_Attribute (Aliases(Alias),
                                                       "Address")) then
                  Ok := False;
                end if;
              end loop;
            end;
          else
            Log_Error ("Config.Init", "Unexpected XML structure",
                       Ctx.Get_Name (Node));
            raise Config_Error;
          end if;
        end loop;
      end loop;
      if not Ok then
        raise Config_Error;
      end if;
    end;
    Debug ("Config checked OK");

  exception
    when Xml_Parser.File_Error =>
      Log_Error ("Config.Init", "File not found",
                 "when parsing config file " & Environ.Getenv (Env_File_Name));

      raise Config_Error;
  end Init;

  -- Get for the bus the heartbeat period, the heartbeat max missed number
  -- and the timeout of connection and send
  -- May raise Config_Error
  procedure Get_Tuning (Name : in String;
                        Heartbeat_Period : out Duration;
                        Heartbeat_Max_Missed : out Positive;
                        Timeout : out Duration;
                        Ttl : out Socket.Ttl_Range) is
    Root : Xml_Parser.Element_Type;
    Crit : Bus_Conf_Rec;
    Found : Boolean;
    Dur : Duration;
    use type As.U.Asu_Us;
  begin
    Init;
    Root := Ctx.Get_Root_Element;

    -- See if this bus is described
    Crit.Addr := As.U.Tus (Bus_Id (Name));
    Found := Bus_Conf_List.Search_Match (Bus_Conf_Match'Access, Crit,
                                         From => Bus_Conf_List_Mng.Absolute);
    Debug ("Bus " & Name & " "
           & (if not Found then "not " else "") & "found for config");

    -- Get Heartbeat_Period
    -----------------------
    Dur := 0.0;
    if Found then
      -- In Conf?
      Dur := Get_Attribute (Bus_Conf_List.Access_Current.Bus,
                            Heartbeat_Period_Name, True);
    end if;
    if Dur = 0.0 then
      -- In Root?
      Dur := Get_Attribute (Root,
                            Heartbeat_Period_Name, True);
    end if;
    if Dur = 0.0 then
      -- Default
      Dur := Default_Heartbeat_Period;
    end if;
    Heartbeat_Period := Dur;

    -- Get Heartbeat_Max_Missed
    Dur := 0.0;
    if Found then
      -- In Conf?
      Dur := Get_Attribute (Bus_Conf_List.Access_Current.Bus,
                            Heartbeat_Max_Missed_Name, False);
    end if;
    if Dur = 0.0 then
      -- In Root?
      Dur := Get_Attribute (Root,
                            Heartbeat_Max_Missed_Name, False);
    end if;
    -- Default or value
    Heartbeat_Max_Missed := (if Dur = 0.0 then Default_Heartbeat_Max_Missed
                             else Positive (Dur));

    -- Get Timeout
    --------------
    Dur := 0.0;
    if Found then
      -- In Conf?
      Dur := Get_Attribute (Bus_Conf_List.Access_Current.Bus,
                            Timeout_Name, True);
    end if;
    if Dur = 0.0 then
      -- In Root?
      Dur := Get_Attribute (Root,
                            Timeout_Name, True);
    end if;
    if Dur = 0.0 then
      -- Default
      Dur := Default_Timeout;
    end if;
    Timeout := Dur;

    -- Get TTL
    Dur := 0.0;
    if Found then
      -- In Conf?
      Dur := Get_Attribute (Bus_Conf_List.Access_Current.Bus,
                            Ttl_Name, False);
    end if;
    if Dur = 0.0 then
      -- In Root?
      Dur := Get_Attribute (Root, Ttl_Name, False);
    end if;
    -- Default or value
    Ttl := (if Dur = 0.0 then Default_Ttl else Positive (Dur));
  end Get_Tuning;

  -- Get Host_Id corerspondig to the address that is attribute of a node
  function Id_Of (Node : Xml_Parser.Element_Type; Attribute : String)
                 return Socket.Host_Id is
  begin
    return Ip_Addr.Parse (Ctx.Get_Attribute (Node, Attribute)).Id;
  end Id_Of;

  -- Return the Host_Id denoting the interface to use
  function Get_Interface (Name : String) return Socket.Host_Id is
    Crit : Bus_Conf_Rec;
    Found : Boolean;
    Local_Host_Name : constant String := Socket.Local_Host_Name;
    Bus, Node : Xml_Parser.Element_Type;
  begin
    Init;
    -- See if this bus is described
    Crit.Addr := As.U.Tus (Bus_Id (Name));
    Found := Bus_Conf_List.Search_Match (Bus_Conf_Match'Access, Crit,
                                         From => Bus_Conf_List_Mng.Absolute);
    Debug ("Bus " & Name & " "
           & (if not Found then "not " else "") & "found for LAN or Alias");

    if not Found then
      return Socket.Local_Host_Id;
    end if;

    -- See if a Alias or LAN for this host
    Bus := Bus_Conf_List.Access_Current.Bus;
    for I in 1 .. Ctx.Get_Nb_Children (Bus) loop
      Node := Ctx.Get_Child (Bus, I);
      if Ctx.Get_Name (Node) = "Aliases" then
        -- Check Alias definitions
        declare
          Aliases : constant Xml_Parser.Nodes_Array
                  := Ctx.Get_Children(Node);
        begin
          for Alias in Aliases'Range loop
            if Ctx.Get_Attribute (Aliases(Alias), "Name") = Local_Host_Name then
              return Id_Of (Aliases(Alias), "Address");
            end if;
          end loop;
        end;
      elsif Ctx.Get_Name (Node) = "LANs" then
        -- Check LAN definitions
        declare
          Lans : constant Xml_Parser.Nodes_Array
               := Ctx.Get_Children(Node);
        begin
          for Lan in Lans'Range loop
            -- See if current LAN/Netmask is one of current interfaces
            begin
              return Socket.Host_Id_For (
                Lan     => Id_Of (Lans(Lan), "Address"),
                Netmask => Id_Of (Lans(Lan), "Netmask"));
            exception
              when Socket.Soc_Name_Not_Found =>
                -- LAN not found in local interface, go on
                null;
            end;
          end loop;
        end;
      end if;
    end loop;
    -- Not found, return the interface associated to host name
    return Socket.Local_Host_Id;
  end Get_Interface;

end Config;

