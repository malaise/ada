with Ada.Calendar;
with As.U.Utils, Reg_Exp;
with Rules;
package Searcher is

  -- Search the Pattern in the Tail last lines and last seconds of File
  -- Clear and set the list to the matching lines
  procedure Search (File_Name : in String;
                    Start     : in Ada.Calendar.Time;
                    Tail      : in Rules.Tail_Length;
                    Aging     : in Duration;
                    Time_Fmt  : in As.U.Asu_Us;
                    Pattern   : not null access Reg_Exp.Compiled_Pattern;
                    Matches   : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type);

end Searcher;

