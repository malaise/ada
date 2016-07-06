-- with As.U, Long_Longs, Timers, Queues, Regular_Expressions;

package body Filters is

  -- Circular list to store history
  -- package Hist_Mng is new Queues.Circ (As.U.Asu_Us);

  -- Tail length
  -- subtype Tail_Length is Long_Longs.Ll_Positive;

  -- type Filter_Rec is record
  --   File : As.U.Asu_Us;
  --   Period : Timers.Period_Range;
  --   Tail : Long_Longs.Ll_Positive;
  --   Hist : access Hist_Mng.Circ_Type;
  --   Rule : As.U.Asu_Us;
  --   Pattern : access Regular_Expressions.Compiled_Pattern;
  -- end record;

  -- Check and store a filter
  -- File_Not_Found : exception;

  procedure Store (Filter : Filter_Rec) is
  begin
    null;
  end Store;

  -- Get the number of stored filters
  function Get_Number return Long_Longs.Ll_Natural is
  begin
    return Get_Number;
  end Get_Number;

  -- Retrieve a filter
  -- No_Filter : exception;

  function Get_Filter (Number : in Long_Longs.Ll_Positive) return Filter_Rec is
  begin
    return Get_Filter (Number);
  end Get_Filter;

end Filters;

