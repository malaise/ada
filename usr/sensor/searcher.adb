-- with As.U.Utils, Regular_Expressions;

-- with Filters;

package body Searcher is

  -- Search the Pattern in the Tail last lines of File
  -- Clear and set the list to the matching lines
  procedure Search (File : in String;
                    Tail : in Filters.Tail_Length;
                    Pattern : access Regular_Expressions.Compiled_Pattern;
                    Matches : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type) is
  begin
    null;
  end Search;

end Searcher;

