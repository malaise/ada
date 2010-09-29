with As.U; use As.U;
with Variables, Tree;
package Matcher is
  -- Expand Node.Text (maybe Check_Only)
  -- See if Str matches Node.Text: string comparison if not Node.Regex
  --  or Regular_Expressions.Exec)
  function Match (Node : Tree.Node_Rec;
                  Str : Asu_Us;
                  Check_Only : Boolean := False) return Boolean;

  Expand_Error : exception renames Variables.Expand_Error;
  Regexp_Error : exception;
  Assign_Error : exception;

end Matcher;

