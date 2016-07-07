with As.U, Long_Longs, Timers, Queues, Regular_Expressions;
package Filters is

  -- Circular list to store history
  package Hist_Mng is new Queues.Circ (As.U.Asu_Us);

  -- Tail length
  subtype Tail_Length is Long_Longs.Ll_Positive;

  type Filter_Rec is record
    File : As.U.Asu_Us;
    Period : Timers.Period_Range;
    Tail : Tail_Length;
    History : access Hist_Mng.Circ_Type;
    Rule : As.U.Asu_Us;
    Pattern : access Regular_Expressions.Compiled_Pattern;
  end record;

  -- Check and store a filter
  File_Not_Found : exception;
  procedure Store (Filter : Filter_Rec);

  -- Get the number of stored filters
  function Get_Number return Long_Longs.Llu_Natural;

  -- Retrieve a filter
  No_Filter : exception;
  function Get_Filter (Number : in Long_Longs.Llu_Positive) return Filter_Rec;

end Filters;

