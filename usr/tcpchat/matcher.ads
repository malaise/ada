with As.U;
with Tree;
package Matcher is
  -- If Node.Eval is Resolve or Compute, then
  -- - Expand Node.Critext in mode check
  -- - If Node.Kind leads to define Node.Expression
  --   (Condif | Repeat | Parse | Set | Eval),
  --    then expand it in mode check
  -- Compute Node.Assign properties from Assign string
  procedure Check (Node : in out Tree.Node_Rec;
                   Assign : in As.U.Asu_Us);
  -- If Node.Eval is Resolve or Compute, then
  -- - Expand Node.Critext. If Compute and Node.Oper is not Match nor Notmatch
  --   then compute it
  -- - If Node.Kind leads to define Node.Expression
  --   (Condif | Repeat | Parse | Set | Eval), then expand or compute it
  --   otherwide do the same for Str
  -- See if Critext matches the expression: string comparison, Regex match
  --  or integer comparison
  -- Assign variables
  function Match (Node : Tree.Node_Rec;
                  Str : As.U.Asu_Us) return Boolean;

  Match_Error : exception;

end Matcher;

