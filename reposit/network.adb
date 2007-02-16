with Unchecked_Deallocation;
package body Network is

  procedure Set_Connection (To : out Connection_Info_Type;
                            Val : in Connection_Info_Type) is
  begin
    To := Val;
  end Set_Connection;

  ----------
  -- Node --
  ----------
  -- Set name of a node
  procedure Set_Name (Of_Node : in out Node_Type; Name : in String) is
  begin
    Of_Node.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
  end Set_Name;

  -- Returns the name of a node
  function Get_Name (Of_Node : Node_Type) return String is
  begin
    return Ada.Strings.Unbounded.To_String (Of_Node.Name);
  end Get_Name;

  -- Set data of a node
  procedure Set_Data (Of_Node : in out Node_Type; Data : in Node_Data_Type) is
  begin
    Of_Node.Data := Data;
    -- Inform the node of node data change
    if Of_Node.Process_Node_Data_Change /= null then
      Of_Node.Process_Node_Data_Change (Of_Node'Unchecked_Access);
    end if;
  end Set_Data;

  -- Returns the data of a node
  function Get_Data (Of_Node : Node_Type) return Node_Data_Type is
  begin
    return Of_Node.Data;
  end Get_Data;


  ----------------
  -- Connection --
  ----------------
  -- Init the connection list if needed
  procedure Init_Connections (Of_Node : in out Node_Type) is
  begin
    if Of_Node.Connections = null then
      Of_Node.Connections := new Connection_Mng.List_Type;
    end if;
  end Init_Connections;

  -- Connects Of_Node to To_Node (both become connected to each other)
  procedure Connect_To (Of_Node : in out Node_Type;
                        To_Node : in out Node_Type;
                        Of_Conn_Data : in Conn_Data_Type;
                        To_Conn_Data : in Conn_Data_Type) is
    Connection_Info : Connection_Info_Type;
  begin
    -- Create a list of connections if none yet
    Init_Connections (Of_Node);
    Init_Connections (To_Node);
    -- Append To_Node to connections of Of_Node,
    -- Connection access is not set yet
    Connection_Info.Node := To_Node'Unchecked_Access;
    Connection_Info.Data := Of_Conn_Data;
    if not Connection_Mng.Is_Empty (Of_Node.Connections.all) then
      Connection_Mng.Rewind (Of_Node.Connections.all, Connection_Mng.Prev);
    end if;
    Connection_Mng.Insert (Of_Node.Connections.all, Connection_Info);
    -- Append Of_Node to connections of To_Node
    Connection_Info.Node := Of_Node'Unchecked_Access;
    Connection_Info.Data := To_Conn_Data;
    Connection_Info.Connection :=
             Connection_Mng.Access_Current (Of_Node.Connections.all);
    if not Connection_Mng.Is_Empty (To_Node.Connections.all) then
      Connection_Mng.Rewind (To_Node.Connections.all, Connection_Mng.Prev);
    end if;
    Connection_Mng.Insert (To_Node.Connections.all, Connection_Info);
    -- Update connection access in Of_Node
    Connection_Mng.Access_Current (Of_Node.Connections.all).Connection :=
          Connection_Mng.Access_Current (To_Node.Connections.all);
    -- Inform both nodes of connection data change
    if Of_Node.Process_Connection_Data_Change /= null then
      Of_Node.Process_Connection_Data_Change (Of_Node'Unchecked_Access,
          Connection_Mng.List_Length (Of_Node.Connections.all));
    end if;
    if To_Node.Process_Connection_Data_Change /= null then
      To_Node.Process_Connection_Data_Change (To_Node'Unchecked_Access,
          Connection_Mng.List_Length (To_Node.Connections.all));
    end if;
  end Connect_To;

  -- Get the number of connections of of_Node
  function Nb_Connections (Of_Node : Node_Type) return Connection_Number is
  begin
    if Of_Node.Connections = null then
      return 0;
    else
      return Connection_Mng.List_Length (Of_Node.Connections.all);
    end if;
  end Nb_Connections;

  -- Lists the connections of Of_Node
  function List_Connections (Of_Node : in Node_Type) return Connection_Array is
    Connections : Connection_Array(
          1 .. Connection_Mng.List_Length (Of_Node.Connections.all));
    Connection : Connection_Info_Type;
    Moved : Boolean;
  begin
    if Connections'Length = 0 then
      -- No rewind nor copy if empty
      return Connections;
    end if;
    -- Rewind and copy
    Connection_Mng.Rewind (Of_Node.Connections.all);
    for I in Connections'Range loop
      Connection_Mng.Read (Of_Node.Connections.all, Connection,
                           Done => Moved);
      Connections(I) := (Node => Connection.Node, Data => Connection.Data);
    end loop;
    return Connections;
  end List_Connections;


  -- Change the data associated to a connection on a node
  -- Raises No_Connection if incorrect index
  procedure Set_Data (Of_Node : in out Node_Type;
                      Index : in Connection_Index;
                      Of_Conn_Data : Conn_Data_Type) is
  begin
    -- Check Index is valid
    if Index > Nb_Connections (Of_Node) then
      raise No_Connection;
    end if;
    -- Move to the proper connection and update data
    Connection_Mng.Move_To (Of_Node.Connections.all, Number => Index - 1,
                            From_Current => False);
    Connection_Mng.Access_Current (Of_Node.Connections.all).Data :=
                    Of_Conn_Data;
    -- Inform the node of connection data change
    if Of_Node.Process_Connection_Data_Change /= null then
      Of_Node.Process_Connection_Data_Change (Of_Node'Unchecked_Access, Index);
    end if;
  end Set_Data;


  -- Delete the connection that Of_Node has with With_Node
  procedure Asym_Delete_Connection (Of_Node : in out Node_Type;
                                    Connection : in Connection_Access) is
    Connection_Read : Connection_Info_Type;
    Length : Positive;
    Moved : Boolean;
  begin
    -- There shall be a connection
    if Nb_Connections (Of_Node) = 0 then
       raise Asym_Connection;
    end if;
    -- Rewind and loop
    Length := Connection_Mng.List_Length (Of_Node.Connections.all);
    Connection_Mng.Rewind (Of_Node.Connections.all);
    loop
      Connection_Mng.Read (Of_Node.Connections.all, Connection_Read,
                           Move => Connection_Mng.Current);
      if Connection_Read.Connection = Connection then
        -- This entry matches: delete it and return
        Connection_Mng.Delete (Of_Node.Connections.all, Done => Moved);
        return;
      end if;
      -- Move to next entry if any
      exit when Connection_Mng.Get_Position (Of_Node.Connections.all) = Length;
      Connection_Mng.Move_To (Of_Node.Connections.all);
    end loop;
    -- There shall have been a connection found and deleted
    raise Asym_Connection;
  end Asym_Delete_Connection;


  -- Checks if Of_Node has a connection to With_Node
  function Has_Connection (Of_Node : Node_Type;
                           With_Node : Node_Access) return Boolean is
    Length : Positive;
  begin
    -- There shall be a connection
    if Nb_Connections (Of_Node) = 0 then
      return False;
    end if;
    -- Rewind and loop
    Length := Connection_Mng.List_Length (Of_Node.Connections.all);
    Connection_Mng.Rewind (Of_Node.Connections.all);
    loop
      if Connection_Mng.Access_Current (Of_Node.Connections.all).Node
               = With_Node then
        -- This entry matches
        return True;
      end if;
      -- Move to next entry if any
      exit when Connection_Mng.Get_Position (Of_Node.Connections.all) = Length;
      Connection_Mng.Move_To (Of_Node.Connections.all);
    end loop;
    -- No connection found
    return False;
  end Has_Connection;

  -- Delete a connection of Of_Node
  -- Raises No_Connection if incorrect index
  procedure Delete_Connection (Of_Node : in out Node_Type;
                               Index : in Connection_Index) is
    Moved : Boolean;
    Connection : Connection_Info_Type;
  begin
    -- Check Index is valid
    if Index > Nb_Connections (Of_Node) then
      raise No_Connection;
    end if;
    -- Move to the proper connection and read it
    Connection_Mng.Move_To (Of_Node.Connections.all, Number => Index - 1,
                            From_Current => False);
    Connection_Mng.Read (Of_Node.Connections.all, Connection,
                           Move => Connection_Mng.Current);
    -- Delete one partner's connection to us
    Asym_Delete_Connection (Connection.Node.all,
                    Connection_Mng.Access_Current(Of_Node.Connections.all));
    -- Delete this connection to partner
    Connection_Mng.Delete (Of_Node.Connections.all, Done => Moved);
  end Delete_Connection;


  procedure Delete_Connections (Of_Node : in out Node_Type;
                                With_Node : in out Node_Type) is
    Connection : Connection_Info_Type;
  begin
    -- Nothing if no connection
    if Nb_Connections (Of_Node) = 0 then
       return;
    end if;
    -- Rewind and loop
    Connection_Mng.Rewind (Of_Node.Connections.all);
    loop
      Connection_Mng.Read (Of_Node.Connections.all, Connection,
                           Move => Connection_Mng.Current);
      if Connection.Node = With_Node'Unchecked_Access then
        -- This entry matches
        -- Delete one partner's connection to us
        Asym_Delete_Connection (With_Node,
              Connection_Mng.Access_Current (Of_Node.Connections.all));
        -- Delete this connection to partner, and done if last
        if Connection_Mng.Get_Position (Of_Node.Connections.all) /=
           Connection_Mng.List_Length (Of_Node.Connections.all) then
          Connection_Mng.Delete (Of_Node.Connections.all);
          exit when Connection_Mng.Is_Empty (Of_Node.Connections.all);
        else
          Connection_Mng.Delete (Of_Node.Connections.all, Connection_Mng.Prev);
          exit;
        end if;
      else
        -- This entry does not match, move to next if any
        exit when Connection_Mng.Get_Position (Of_Node.Connections.all) =
                  Connection_Mng.List_Length (Of_Node.Connections.all);
        Connection_Mng.Move_To (Of_Node.Connections.all);
      end if;
    end loop;
    -- There shall be no more connection of With_Node to Of_Node
    if Has_Connection (With_Node, Of_Node'Unchecked_Access) then
      raise Asym_Connection;
    end if;
  end Delete_Connections;


  -- Delete all the connections (if any) that Of_Node
  procedure Delete_All_Connections (Of_Node : in out Node_Type) is
    Connection : Connection_Info_Type;
  begin
    -- Nothing if no connection
    if Nb_Connections (Of_Node) = 0 then
       return;
    end if;
    -- Loop of rewind and delete connections with first partner
    loop
      Connection_Mng.Rewind (Of_Node.Connections.all);
      Connection_Mng.Read (Of_Node.Connections.all, Connection,
                           Move => Connection_Mng.Current);
      Delete_Connections (Of_Node, Connection.Node.all);
      exit when Connection_Mng.Is_Empty (Of_Node.Connections.all);
    end loop;
  end Delete_All_Connections;

  -----------------------
  -- Automatic actions --
  -----------------------
  -- Set the processing of a incomming message
  procedure Set_Process_Message (On_Node : in out Node_Type;
                                 Process_Message : in Process_Message_Type) is
  begin
    On_Node.Process_Message := Process_Message;
  end Set_Process_Message;

  -- Send a message from a node to a node
  procedure Send_Message (From_Node : in out Node_Type;
                          To_Node   : in out Node_Type;
                          Message   : in Message_Type) is
  begin
    -- Make To_Node process the message
    if To_Node.Process_Message = null then
      return;
    else
      To_Node.Process_Message (From_Node'Unchecked_Access, Message);
    end if;
  end Send_Message;

  -- Set processing of change of node data
  procedure Set_Process_Node_Data_Change (
      On_Node : in out Node_Type;
      Process_Node_Data_Change : in Process_Node_Data_Change_Type) is
  begin
    On_Node.Process_Node_Data_Change := Process_Node_Data_Change;
  end Set_Process_Node_Data_Change;

  -- Set processing of change of connection data
  procedure Set_Process_Connection_Data_Change (
      On_Node : in out Node_Type;
      Process_Connection_Data_Change : in Process_Connection_Data_Change_Type)
      is
  begin
    On_Node.Process_Connection_Data_Change := Process_Connection_Data_Change;
  end Set_Process_Connection_Data_Change;

  -- Finalization
  procedure Finalize (A_Node : in out Node_Type) is
    procedure Deallocation_Of is new
      Unchecked_Deallocation (Object => Connection_Mng.List_Type,
                              Name => Connections_Access);
    Name : constant String := A_Node.Get_Name;
  begin
    -- Disconnect from network
    Delete_All_Connections (A_Node);
    -- Release memory, deallocate the list
    if A_Node.Connections /= null then
      Connection_Mng.Delete_List (A_Node.Connections.all, Deallocate => True);
      Deallocation_Of (A_Node.Connections);
    end if;
  exception
    when others =>
      null;
  end Finalize;

end Network;


