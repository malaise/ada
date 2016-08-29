with As.U.Utils, Reg_Exp;
with Filters;
package Searcher is

  -- Search the Pattern in the Tail last lines and las seconds of File
  -- Clear and set the list to the matching lines
  procedure Search (File_Name : in String;
                    Tail      : in Filters.Tail_Length;
                    Seconds   : in Filters.Tail_Length;
                    Time_Fmt  : in As.U.Asu_Us;
                    Pattern   : access Reg_Exp.Compiled_Pattern;
                    Matches   : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type);

end Searcher;

