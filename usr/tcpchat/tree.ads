with As.U;
with Trees, Event_Mng, Property_Def;
package Tree is

  -- Parse file and build tree
  procedure Parse (File_Name : in As.U.Asu_Us);
  Parse_Error : exception;

  -- Kind of node
  type Node_Kind is (Selec, Expect, Default, Timeout, Cond, Condif, Condelse,
                     Repeat, Read, Skip, Wait, Send, Log, Call, Eval,
                     Set, Parse, Chdir, Close);

  -- Infinite timeout
  Infinite_Ms : constant Integer := Event_Mng.Infinite_Ms;

  -- Max nb of assignments
  Max_Assignments : constant Positive := 9;
  subtype Assignments_Range is Positive range 1 .. Max_Assignments;
  subtype Assignments is Property_Def.Properties (Assignments_Range);

  -- Operation list
  type Oper_List is (Equal, Noteq, Match, Notmatch,
                     Greater, Less, Greatereq, Lesseq);

  -- Evaluations rule
  type Eval_List is (None, Resolve, Compute);

  -- Node
  type Position_Access;
  type Node_Access is access Position_Access;
  type Node_Rec is record
    Kind : Node_Kind := Close;
    -- For chat (kind Expect)
    Name : As.U.Asu_Us;
    -- The criteria to match or the text
    Critext : As.U.Asu_Us;
    -- The timeout for read or expect
    Timeout : Integer := Infinite_Ms;
    -- How to interprete the condition
    Eval : Eval_List := Resolve;
    -- The expression to check
    Expression : As.U.Asu_Us;
    -- The operation
    Oper : Oper_List := Equal;
    Ifunset : Boolean := False;
    -- The assignements
    Assign : Assignments;
    -- Next statement
    Next : Node_Access := null;
  end record;

  -- The tree of chats
  package Tree_Mng is new Trees.Tree (Node_Rec);
  Chats : Tree_Mng.Tree_Type;
  type Position_Access is new Tree_Mng.Position_Access;
  No_Position : constant Position_Access
              := Position_Access (Tree_Mng.No_Position);

  -- Get the version of chats file
  function Get_Version return String;

  -- Set current node of Chats to a new position
  procedure Set_Position (Position : in Position_Access);

end Tree;

