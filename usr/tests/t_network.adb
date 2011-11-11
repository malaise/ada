with Ada.Exceptions;
with Basic_Proc, Network;
procedure T_Network is
  package Mnet is new Network (Integer, Integer, Integer);
  N1, N2, N3 : Mnet.Node_Type;

  -- Display connections of N
  procedure Put_Connections (N : Mnet.Node_Type) is
    N_Conn : constant Mnet.Connection_Array := N.List_Connections;
  begin
    Basic_Proc.Put_Output (N.Get_Name & " -> ");
    for I in N_Conn'Range loop
      Basic_Proc.Put_Output (N_Conn(I).Node.Get_Name
                           & " " & N_Conn(I).Data'Img & ", ");
    end loop;
    Basic_Proc.New_Line_Output;
  end Put_Connections;

begin
  -- Create three nodes and connect them
  Basic_Proc.Put_Line_Output ("Creating Node1 to 3.");
  N1.Set_Name ("Node1");
  N1.Set_Data (1);
  N2.Set_Name ("Node2");
  N2.Set_Data (2);
  N3.Set_Name ("Node3");
  N3.Set_Data (3);
  Basic_Proc.Put_Line_Output ("Connecting 1-2 1-3 2-3 3-2");
  N1.Connect_To (N2, 2, 1);
  N1.Connect_To (N3, 3, 1);
  N2.Connect_To (N3, 3, 2);
  N3.Connect_To (N2, 2, 3);
  -- Display connections
  Put_Connections (N1);
  Put_Connections (N2);
  Put_Connections (N3);

  Basic_Proc.Put_Line_Output ("Creating temporary Node4 connected with Node2");
  declare
    N4 : Mnet.Node_Type;
  begin
    N4.Set_Name ("Node4");
    N4.Set_Data (4);
    N4.Connect_To (N2, 2, 4);
  end;

  -- Delete a connection
  Basic_Proc.Put_Line_Output ("Deleting first connection of Node1");
  N1.Delete_Connection (1);
  -- Display connections
  Put_Connections (N1);
  Put_Connections (N2);
  Put_Connections (N3);

  -- Delete all connections between two nodes
  Basic_Proc.Put_Line_Output (
        "Deleting all connections between Node2 and Node3");
  N2.Delete_Connections (N3);
  -- Display connections
  Put_Connections (N1);
  Put_Connections (N2);
  Put_Connections (N3);

  -- Delete all connections
  Basic_Proc.Put_Line_Output ("Deleting all connections of Node3");
  N3.Delete_All_Connections;
  -- Display connections
  Put_Connections (N1);
  Put_Connections (N2);
  Put_Connections (N3);

  -- Done
  Basic_Proc.Put_Line_Output ("Done.");
exception
  when Error:others =>
    Basic_Proc.Put_Line_Output ("Exception raised: "
                         & Ada.Exceptions.Exception_Name (Error));
end T_Network;

