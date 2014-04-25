with As.U;
with Tree;
package Matcher is
  -- Expand Node.Critext in mode check
  -- If Regepxp, then compile and test regex with a Dummy text
  -- Compute Node.Assign properties from Assign string
  procedure Check (Node : in out Tree.Node_Rec;
                   Assign : in As.U.Asu_Us);
  -- Expand Node.Critext and Str
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec) or integer comparison
  -- Assign variables
  function Match (Node : Tree.Node_Rec;
                  Str : As.U.Asu_Us) return Boolean;

  Match_Error : exception;

end Matcher;

