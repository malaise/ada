with Sys_Calls, Long_Long_Limited_List;
package body Filters is

  procedure Set (To : out Filter_Rec; Val : in Filter_Rec) is
  begin
    To := Val;
  end Set;

  package Filters_Mng is new Long_Long_Limited_List (Filter_Rec, Set);
  Filters : Filters_Mng.List_Type;

  -- type Filter_Rec is record
  --   File : As.U.Asu_Us;
  --   Period : Timers.Period_Range;
  --   Tail : Long_Longs.Ll_Positive;
  --   Hist : access Hist_Mng.Circ_Type;
  --   Rule : As.U.Asu_Us;
  --   Pattern : access Reg_Exp.Compiled_Pattern;
  -- end record;

  -- Check and store a filter
  procedure Store (Filter : Filter_Rec) is
  begin
    if not Sys_Calls.File_Check (Filter.File.Image) then
      raise File_Not_Found;
    end if;
    Filters.Rewind (Filters_Mng.Prev, Check_Empty => False);
    Filters.Insert (Filter);
  end Store;

  -- Get the number of stored filters
  function Get_Number return Long_Longs.Ll_Natural is
  begin
    return Long_Longs.Ll_Natural (Filters.List_Length);
  end Get_Number;

  -- Retrieve a filter
  function Get_Filter (Number : in Long_Longs.Ll_Positive) return Filter_Rec is
    use type Long_Longs.Llu_Natural;
  begin
    if Long_Longs.Llu_Positive (Number) > Filters.List_Length then
      raise No_Filter;
    end if;
    -- Read the filter
    Filters.Move_At (Long_Longs.Llu_Positive (Number));
    return Filter : Filter_Rec do
      Filters.Read (Filter, Filters_Mng.Current);
    end return;
  end Get_Filter;

end Filters;

