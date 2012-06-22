with As.U, Trilean;
with Trees, Event_Mng, Property_Def;
package Tree is

  -- Parse file and build tree
  procedure Parse (File_Name : in As.U.Asu_Us);
  Parse_Error : exception;

  -- Kind of node
  type Node_Kind is (Nop, Selec, Cond, Condif, Condelse, Repeat, Read, Default,
                     Skip, Wait, Send, Call, Eval, Set, Chdir, Log, Close);

  -- Infinite timeout
  Infinite_Ms : constant Integer := Event_Mng.Infinite_Ms;

  -- Max nb of assignments
  Max_Assignments : constant Positive := 9;
  subtype Assignments_Range is Positive range 1 .. Max_Assignments;
  subtype Assignments is Property_Def.Properties (Assignments_Range);

  -- Node
  type Position_Access;
  type Node_Access is access Position_Access;
  type Node_Rec is record
    Kind : Node_Kind := Nop;
    -- For chat (kind Read)
    Name : As.U.Asu_Us;
    -- For Cond, read, send, call, eval or chdir
    Text : As.U.Asu_Us;
    -- For Selec, Read, Skip, Wait
    Timeout : Integer := Infinite_Ms;
    -- For chat, cond, read
    Regexp : Boolean := False;
    -- For Set
    Compute : Boolean := False;
    -- For Set, Eval, Condif
    Ifunset : Trilean.Trilean := Trilean.False;
    -- For chat, cond, read, eval
    Assign : Assignments;
    -- Next statement
    Next : Node_Access := null;
  end record;

  package Tree_Mng is new Trees.Tree (Node_Rec);
  Chats : Tree_Mng.Tree_Type;

  type Position_Access is new Tree_Mng.Position_Access;
  No_Position : constant Position_Access
              := Position_Access(Tree_Mng.No_Position);

  procedure Set_Position (Position : in Position_Access);

  function Get_Version return String;
end Tree;

