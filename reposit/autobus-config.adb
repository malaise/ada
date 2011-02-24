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
  Default_Heartbeat_Missed_Number : constant := 3;
  Default_Connection_Timeout : constant Duration := 0.5;

  -- Dynamic list of pair of host Ids
  type Pair_Rec is record
    Host1, Host2 : Socket.Host_Id;
  end record;
  package Pair_Dyn_List_Mng is new Dynamic_List (Pair_Rec);
  package Pair_List_Mng renames Pair_Dyn_List_Mng.Dyn_List;
  function Pair_Match (Curr, Crit : Pair_Rec) return Boolean is
    use type Socket.Host_Id;
  begin
    return  (Curr.Host1 = Crit.Host1 and then Curr.Host2 = Crit.Host2)
    or else (Curr.Host1 = Crit.Host2 and then Curr.Host2 = Crit.Host1);
  end Pair_Match;

  -- Init (parse the file)
  procedure Init is
    Root, Conf, Connection : Xml_Parser.Element_Type;
    New_Node : Xml_Parser.Node_Type;
    Host_Pair_Attr : Xml_Parser.Attributes_Array(1 .. 2);
    Host : Tcp_Util.Remote_Host;
    Host_Pair_Id : Pair_Rec;
    Pair_List : Pair_List_Mng.List_Type;
    Ok : Boolean;
    use type As.U.Asu_Us;
    use type Xml_Parser.Ctx_Status_List;
  begin
    -- Check if already init
    if Ctx.Get_Status /= Xml_Parser.Clean then
      return;
    end if;

    -- Make dummy tree if env variable is set
    if not Environ.Is_Set (Env_File_Name) then
      Ctx.Set_Version (1, 0);
      Root := Ctx.Get_Root_Element;
      Ctx.Set_Name (Root, "Autobus");
      Ctx.Check (Ok);
      if not Ok then
        Log_Error ("Config.Init", Ctx.Get_Parse_Error_Message,
                   "on out dummy tree");
        raise Internal_Error;
      end if;
      Debug ("Config.Init dummy tree cause no Env");
      return;
    end if;

    -- Parse file, get Root
    Ctx.Parse (Environ.Getenv (Env_File_Name), Ok);
    if not Ok then
      Log_Error ("Config.Init", Ctx.Get_Parse_Error_Message,
                   "when parsing config file");
        raise Config_Error;
    end if;
    Root := Ctx.Get_Root_Element;


    -- For each Conf, scan each Pair of each Connection
    --  * replace any Host name by its address, remove Pair if
    --    a name is not found
    --  * Make a list of the Pairs (hostIds) defined for this Conf entry
    --  * Check unicity of these Pairs
    All_Configs:
    for I in 1 ..  Ctx.Get_Nb_Children (Root) loop
      -- Go to first/next Conf
      if I = 1 then
        Conf := Ctx.Get_Child (Root, 1);
      else
        Conf := Ctx.Get_Brother (Conf);
      end if;
      declare
        Attrs : constant Xml_Parser.Attributes_Array
              := Ctx.Get_Attributes (Conf);
        Txt : As.U.Asu_Us;
      begin
        for I in Attrs'Range loop
          Txt.Append (Attrs(I).Name & "=" & Attrs(I).Value & " ");
        end loop;
        Debug ("Config.Init got Config " & Txt.Image);
      end;

      -- Skip the Buses, there is at least one, to move to first Connection
      Connection := Ctx.Get_Child (Conf, 1);
      loop
        Debug ("Config.Init got bus "
             & Ctx.Get_Attribute(Connection, 1).Value.Image);
        Connection := Ctx.Get_Brother (Connection);
        exit when Ctx.Get_Name (Connection).Image = "Connection";
      end loop;

      -- Iterate on all configs
      Pair_List.Delete_List;
      loop
        Debug ("Config.Init got config "
             & Ctx.Get_Attribute(Connection, 1).Name.Image
             & "=" & Ctx.Get_Attribute(Connection, 1).Value.Image);
        -- Iterate on all pairs
        declare
          Pairs : Xml_Parser.Nodes_Array := Ctx.Get_Children (Connection);
          Save : Boolean;
          use type Tcp_Util.Remote_Host_List;
        begin
          for J in Pairs'Range loop
            begin
              Host_Pair_Attr := Ctx.Get_Attributes (Pairs(J));
              -- Get both Host_Ids
              Save := False;
              Host := Ip_Addr.Parse (Host_Pair_Attr(1).Value.Image);
              if Host.Kind = Tcp_Util.Host_Name_Spec then
                -- Host name
                Host_Pair_Id.Host1 := Socket.Host_Id_Of (Host.Name.Image);
                Host_Pair_Attr(1).Value := As.U.Tus (Ip_Addr.Image (
                        Socket.Id2Addr (Host_Pair_Id.Host1)));
                Save := True;
              else
                -- Host address
                Host_Pair_Id.Host1 := Host.Id;
              end if;
              Host := Ip_Addr.Parse (Host_Pair_Attr(2).Value.Image);
              if Host.Kind = Tcp_Util.Host_Name_Spec then
                -- Host name
                Host_Pair_Id.Host2 := Socket.Host_Id_Of (Host.Name.Image);
                Host_Pair_Attr(2).Value := As.U.Tus (Ip_Addr.Image (
                        Socket.Id2Addr (Host_Pair_Id.Host2)));
                Save := True;
              else
                -- Host address
                Host_Pair_Id.Host2 := Host.Id;
              end if;
              -- Insert HostIds in list
              Pair_List.Insert (Host_Pair_Id);
              -- Replace Attibutes if needed
              if Save then
                Ctx.Set_Attributes (Pairs(J), Host_Pair_Attr);
              end if;
              Debug ("Storing pair " & Host_Pair_Attr(1).Value.Image
                             & " - " & Host_Pair_Attr(2).Value.Image);
            exception
              when Socket.Soc_Name_Not_Found =>
                -- Host name not found => Remove this pair
                Debug ("Dropping pair " & Host_Pair_Attr(1).Value.Image
                                & " - " & Host_Pair_Attr(2).Value.Image);
                Ctx.Delete_Node (Pairs(J), New_Node);
            end;
          end loop;
        end;

        exit when not Ctx.Has_Brother (Connection);
      end loop;

      -- Check Unicity of Pairs
      -- @@@
    end loop All_Configs;
  exception
    when Config_Error | Internal_Error =>
      raise;
    when Except:others =>
      Log_Exception ("Config.Init", Ada.Exceptions.Exception_Name (Except),
                     "parsing tree");
      raise Config_Error;
  end Init;

  -- Get the heartbeat period for the Bus
  function Get_Heartbeat_Period (Bus : String) return Duration is
  begin
    Init;
    return Default_Heartbeat_Period;
  end Get_Heartbeat_Period;

  -- Get the heartbeat missed number for the bus
  function Get_Heartbeat_Missed_Number (Bus : String) return Positive is
  begin
    Init;
    return Default_Heartbeat_Missed_Number;
  end Get_Heartbeat_Missed_Number;

  -- Get Connection_Timeout for the Bus and the target Host (IP address)
  function Get_Connection_Timeout (Bus : String; Dest : String)
             return Duration is
  begin
    return Default_Connection_Timeout;
  end Get_Connection_Timeout;

end Config;

