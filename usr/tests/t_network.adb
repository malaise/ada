with Ada.Text_Io, Ada.Exceptions;
with Network;
procedure T_Network is
  package Mnet is new Network (Integer, Integer, Integer);
  N1, N2, N3 : Mnet.Node_Type;

  -- Display connections of N
  procedure Put_Connections (N : Mnet.Node_Type) is
    N_Conn : constant Mnet.Connection_Array := N.List_Connections;
  begin
    Ada.Text_Io.Put (N.Get_Name & " -> ");
    for I in N_Conn'Range loop
      Ada.Text_Io.Put (N_Conn(I).Node.Get_Name & " " & N_Conn(I).Data'Img & ", ");
    end loop;
    Ada.Text_Io.New_Line;
  end Put_Connections;

begin
  -- Create three nodes and connect them
  Ada.Text_Io.Put_Line ("Creating Node1 to 3.");
  N1.Set_Name ("Node1");
  N1.Set_Data (1);
  N2.Set_Name ("Node2");
  N2.Set_Data (2);
  N3.Set_Name ("Node3");
  N3.Set_Data (3);
  Ada.Text_Io.Put_Line ("Connecting 1-2 1-3 2-3 3-2");
  N1.Connect_To (N2, 2, 1);
  N1.Connect_To (N3, 3, 1);
  N2.Connect_To (N3, 3, 2);
  N3.Connect_To (N2, 2, 3);
  -- Display connections
  Put_Connections (N1);
  Put_Connections (N2);
  Put_Connections (N3);

  Ada.Text_Io.Put_Line ("Creating temporary Node4 connected with Node2");
  declare
    N4 : Mnet.Node_Type;
  begin
    N4.Set_Name ("Node4");
    N4.Set_Data (4);
    N4.Connect_To (N2, 2, 4);
  end;

  -- Delete a connection
  Ada.Text_Io.Put_Line ("Deleting first connection of Node1");
  N1.Delete_Connection (1);
  -- Display connections
  Put_Connections (N1);
  Put_Connections (N2);
  Put_Connections (N3);

  -- Delete all connections between two nodes
  Ada.Text_Io.Put_Line ("Deleting all connections between Node2 and Node3");
  N2.Delete_Connections (N3);
  -- Display connections
  Put_Connections (N1);
  Put_Connections (N2);
  Put_Connections (N3);
  
  -- Delete all connections
  Ada.Text_Io.Put_Line ("Deleting all connections of Node3");
  N3.Delete_All_Connections;
  -- Display connections
  Put_Connections (N1);
  Put_Connections (N2);
  Put_Connections (N3);

  -- Done
  Ada.Text_Io.Put_Line ("Done.");
exception
  when Error:others =>
    Ada.Text_Io.Put_Line ("Exception raised: "
                         & Ada.Exceptions.Exception_Name (Error));
end T_Network;

