-- Network of nodes
with Ada.Finalization;
with As.U; use As.U;
with Limited_List;
generic
  -- The data to store in each node
  type Node_Data_Type is private;
  -- The data to store with each connection
  type Conn_Data_Type is private;
  -- The message exchanged on a connection
  type Message_Type is private;

package Network is

  ----------
  -- NODE --
  ----------
  -- A node
  type Node_Type is tagged limited private;
  type Node_Access is access all Node_Type;
  -- Connection number and index
  subtype Connection_Number is Natural;
  subtype Connection_Index is Connection_Number
               range 1 .. Connection_Number'Last;

  -- Data associated to a connection
  type Connection_Key_Type is private;
  type Connection_Info is record
    Key : Connection_Key_Type;
    Node : Node_Access;
    Data : Conn_Data_Type;
  end record;
  type Connection_Array is array (Connection_Index range <>) of Connection_Info;

  -- Set name of a node
  procedure Set_Name (Of_Node : in out Node_Type; Name : in String);

  -- Returns the name of a node
  function Get_Name (Of_Node : Node_Type) return String;

  -- Set data of a node
  procedure Set_Data (Of_Node : in out Node_Type; Data : in Node_Data_Type);

  -- Returns the data of a node
  function Get_Data (Of_Node : Node_Type) return Node_Data_Type;

  ----------------
  -- Connection --
  ----------------
  -- Connects A_Node to To_Node (both become connected to each other)
  procedure Connect_To (Of_Node : in out Node_Type;
                        To_Node : in out Node_Type;
                        Of_Conn_Data : in Conn_Data_Type;
                        To_Conn_Data : in Conn_Data_Type);

  -- Get the number of connections of of_Node
  function Nb_Connections (Of_Node : Node_Type) return Connection_Number;

  -- Lists the connections of Of_Node
  function List_Connections (Of_Node : Node_Type) return Connection_Array;

  -- Change the data associated to a connection on a node
  -- Raises No_Connection if incorrect index
  procedure Set_Data (Of_Node : in out Node_Type;
                      Index : in Connection_Index;
                      Of_Conn_Data : in Conn_Data_Type);

  -- Delete a connection of Of_Node, specify index
  --  Beware that connection index may change (when some are deleted)
  --  so deleting by key is safer
  -- Raises No_Connection if incorrect index
  procedure Delete_Connection (Of_Node : in out Node_Type;
                               Index : in Connection_Index);

  -- Delete a connection of Of_Node, specify key
  -- Raises No_Connection if no connection with this key
  procedure Delete_Connection (Of_Node : in out Node_Type;
                               Key : in Connection_Key_Type);

  -- Delete all the connections (if any) between Of_Node and With_node
  procedure Delete_Connections (Of_Node : in out Node_Type;
                                With_Node : in out Node_Type);

  -- Delete all the connections (if any) that Of_Node
  procedure Delete_All_Connections (Of_Node : in out Node_Type);

  -- Raised by Delete_Connection if Index is too big (> List_Connections)
  No_Connection : exception;

  -- Raised by any connection deletion if a asymetric situation is detected
  --  (A is connected to B but B is not connected to A)
  Asym_Connection : exception;

  -------------
  -- Message --
  -------------
  -- Send a message from a node to a node
  procedure Send_Message (From_Node : in out Node_Type;
                          To_Node   : in out Node_Type;
                          Message   : in Message_Type);

  -----------------------
  -- Automatic actions --
  -----------------------
  -- Set the processing of a incomming message
  type Process_Message_Type is access procedure (
      From_Node : in Node_Access;
      Message   : in Message_Type);
  procedure Set_Process_Message (
      On_Node : in out Node_Type;
      Process_Message : in Process_Message_Type);

  -- Set processing of change of node data
  type Process_Node_Data_Change_Type is access procedure (
      On_Node : in Node_Access);
  procedure Set_Process_Node_Data_Change (
      On_Node : in out Node_Type;
      Process_Node_Data_Change : in Process_Node_Data_Change_Type);

  -- Set processing of change of connection data
  type Process_Connection_Data_Change_Type is access procedure (
      On_Node : in Node_Access;
      On_Connection : in Connection_Index);
  procedure Set_Process_Connection_Data_Change (
      On_Node : in out Node_Type;
      Process_Connection_Data_Change : in Process_Connection_Data_Change_Type);

private

  -- A connection
  type Connection_Info_Type;
  type Connection_Access is access all Connection_Info_Type;
  type Connection_Key_Type is new Connection_Access;
  type Connection_Info_Type is record
    -- Remote node and connection
    Node : Node_Access;
    Connection : Connection_Access;
    Data : Conn_Data_Type;
  end record;
  procedure Set_Connection (To : out Connection_Info_Type;
                            Val : in Connection_Info_Type);

  -- List of node's connections (dynamic list of node accesses)
  package Connection_Mng is new Limited_List (Connection_Info_Type,
                                   Connection_Access, Set_Connection);
  type Connections_Access is access Connection_Mng.List_Type;

  -- The exported node type
  type Node_Type is limited new Ada.Finalization.Limited_Controlled with record
    -- Node name. Empty when node is deleted
    Name : Asu_Us;
    -- List of node's connections
    Connections : Connections_Access := null;
    -- Node data
    Data : Node_Data_Type;
    -- Process message procedure
    Process_Message : Process_Message_Type := null;
    -- Process node data change procedure
    Process_Node_Data_Change : Process_Node_Data_Change_Type := null;
    -- Process connection data change procedure
    Process_Connection_Data_Change : Process_Connection_Data_Change_Type := null;
  end record;

  -- Finalization of destruction
  overriding procedure Finalize (A_Node : in out Node_Type);

end Network;

