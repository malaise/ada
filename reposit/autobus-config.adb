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
    Config : Xml_Parser.Node_Type;
  end record;
  function Bus_Conf_Match (Curr, Crit : Bus_Conf_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Curr.Addr = Crit.Addr;
  end Bus_Conf_Match;
  package Bus_Conf_Dyn_List_Mng is new Dynamic_List (Bus_Conf_Rec);
  package Bus_Conf_List_Mng renames Bus_Conf_Dyn_List_Mng.Dyn_List;
  Bus_Conf_List : Bus_Conf_List_Mng.List_Type;

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
                   "when parsing config file");
        raise Config_Error;
    end if;
    Debug ("File " & Environ.Getenv (Env_File_Name) & " parsed OK");
    Root := Ctx.Get_Root_Element;

    -- Check all Buses of all configs, check that names are A-<Ip_Address>
    declare
      Configs : constant Xml_Parser.Nodes_Array
              := Ctx.Get_Children(Root);
      Name : As.U.Asu_Us;
    begin
      Ok := True;
      if Check_Attributes (Root) then
        Debug ("Default config " & Image (Ctx.Get_Attributes (Root)));
      else
        Ok := False;
      end if;

      for C in Configs'Range loop
        declare
          Buses : constant Xml_Parser.Nodes_Array
              := Ctx.Get_Children(Configs(C));
        begin
          if Check_Attributes (Configs(C)) then
            Debug ("Got config " & Image (Ctx.Get_Attributes (Configs(C))));
          else
            Ok := False;
          end if;
          for B in Buses'Range loop
            -- Get bus name
            Name := Ctx.Get_Attribute (Buses(B), 1).Value;
            if Name.Length > 2 and then Name.Slice (1, 2) = "A-" then
              Bus_Conf_List.Insert ( (Name, Buses(B), Configs(C)) );
              Debug ("Bus address " & Name.Image);
            else
              Log_Error ("Config.Init", "invalid bus name", Name.Image);
              Ok := False;
            end if;
          end loop;
        end;
      end loop;
      if not Ok then
        raise Config_Error;
      end if;
    end;

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
    Crit.Addr := As.U.Tus ("A-" & Name);
    Bus_Conf_List.Search_Match (Found, Bus_Conf_Match'Access, Crit,
                                From => Bus_Conf_List_Mng.Absolute);
    Debug ("Bus " & Name & " "
           & (if not Found then "not" else "") & " found in config");

    -- Get Heartbeat_Period
    -----------------------
    Dur := 0.0;
    if Found then
      -- In Conf?
      Dur := Get_Attribute (Bus_Conf_List.Access_Current.Config,
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
      Dur := Get_Attribute (Bus_Conf_List.Access_Current.Config,
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
      Dur := Get_Attribute (Bus_Conf_List.Access_Current.Config,
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
      Dur := Get_Attribute (Bus_Conf_List.Access_Current.Config,
                            Ttl_Name, False);
    end if;
    if Dur = 0.0 then
      -- In Root?
      Dur := Get_Attribute (Root, Ttl_Name, False);
    end if;
    -- Default or value
    Ttl := (if Dur = 0.0 then Default_Ttl else Positive (Dur));
  end Get_Tuning;

end Config;

