with As.U, Long_Longs, Timers, Queues, Reg_Exp;
package Rules is

  -- Circular list to store history
  package Hist_Mng is new Queues.Circ (As.U.Asu_Us);

  -- Tail length
  subtype Tail_Length is Long_Longs.Ll_Natural;

  type Pattern_Access is access all Reg_Exp.Compiled_Pattern;
  type History_Access is access Hist_Mng.Circ_Type;
  type Rule_Rec is record
    File : As.U.Asu_Us;
    Period : Timers.Period_Range;
    Tail : Tail_Length;
    History : History_Access;
    Action : As.U.Asu_Us;
    Seconds : Tail_Length;
    Time_Format : As.U.Asu_Us;
    Pattern : Pattern_Access;
  end record;

  -- Check and store a rule
  File_Not_Found : exception;
  procedure Store (Rule : Rule_Rec);

  -- Get the number of stored rules
  function Get_Number return Long_Longs.Ll_Natural;

  -- Retrieve a rule
  No_Rule : exception;
  function Get_Rule (Number : in Long_Longs.Ll_Positive) return Rule_Rec;

end Rules;

