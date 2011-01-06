with As.U;
with Tree;
package Matcher is
  -- Expand Node.Text in mode check
  -- Compile and test regex with a Dummy text
  -- Compute Node.Assign properties from Assign string
  procedure Check (Node : in out Tree.Node_Rec;
                   Assign : in As.U.Asu_Us);
  -- Expand Node.Text
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec)
  -- Assign variables
  function Match (Node : Tree.Node_Rec;
                  Str : As.U.Asu_Us) return Boolean;

  Match_Error : exception;

end Matcher;

