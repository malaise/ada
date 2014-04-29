with As.U;
with Trees, Event_Mng, Property_Def;
package Tree is

  -- Parse file and build tree
  procedure Parse (File_Name : in As.U.Asu_Us);
  Parse_Error : exception;

  -- Kind of node
  type Node_Kind is (Selec, Expect, Cond, Condif, Condelse, Repeat,
                     Read, Default, Timeout, Skip, Wait, Send, Call, Eval,
                     Set, Parse, Chdir, Log, Close);

  -- Infinite timeout
  Infinite_Ms : constant Integer := Event_Mng.Infinite_Ms;

  -- Max nb of assignments
  Max_Assignments : constant Positive := 9;
  subtype Assignments_Range is Positive range 1 .. Max_Assignments;
  subtype Assignments is Property_Def.Properties (Assignments_Range);

  -- Operation list
  type Oper_List is (Equal, Noteq, Greater, Smaller, Greatereq, Smallereq);

  -- Node
  type Node_Rec is record
    Kind : Node_Kind := Close;
    -- For chat (kind Expect)
    Name : As.U.Asu_Us;
    -- The criteria to match or the text
    Critext : As.U.Asu_Us;
    -- The timeout for read or expect
    Timeout : Integer := Infinite_Ms;
    -- How to interprete the condition
    Regexp : Boolean := False;
    Compute : Boolean := False;
    -- The expression to check
    Expression : As.U.Asu_Us;
    -- The operation
    Oper : Oper_List;
    Ifunset : Boolean := False;
    -- The assignements
    Assign : Assignments;
  end record;

  package Tree_Mng is new Trees.Tree (Node_Rec);
  Chats : Tree_Mng.Tree_Type;

  function Get_Version return String;
end Tree;

