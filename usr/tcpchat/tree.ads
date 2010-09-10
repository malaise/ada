with As.U; use As.U;
with Trees, Event_Mng;
package Tree is

  -- Parse file and build tree
  procedure Parse (File_Name : in Asu_Us);
  Parse_Error : exception;

  -- Kind of node
  type Node_Kind is (Selec, Read, Default, Skip, Wait, Send, Call, Close);

  -- Infinite timeout
  Infinite_Ms : constant Integer := Event_Mng.Infinite_Ms;

  -- Node
  type Position_Access;
  type Node_Access is access Position_Access;
  type Node_Rec is record
    Kind : Node_Kind := Close;
    -- For chat (kind Read)
    Name : Asu_Us;
    -- For Read, Send, Call or Exec
    Text : Asu_Us;
    -- For Selec, Read, Skip, Wait
    Timeout : Integer := Infinite_Ms;
    -- Next statement
    Next : Node_Access := null;
  end record;

  package Tree_Mng is new Trees.Tree (Node_Rec);
  Chats : Tree_Mng.Tree_Type;

  type Position_Access is new Tree_Mng.Position_Access;

end Tree;

