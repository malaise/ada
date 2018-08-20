with Sys_Calls, Long_Long_Limited_List;
package body Rules is

  procedure Set (To : out Rule_Rec; Val : in Rule_Rec) is
  begin
    To := Val;
  end Set;

  package Rules_Mng is new Long_Long_Limited_List (Rule_Rec, Set);
  Rules : Rules_Mng.List_Type;

  -- type Rule_Rec is record
  --   File : As.U.Asu_Us;
  --   Period : Timers.Period_Range;
  --   Tail : Long_Longs.Ll_Positive;
  --   Hist : access Hist_Mng.Circ_Type;
  --   Action : As.U.Asu_Us;
  --   Pattern : access Reg_Exp.Compiled_Pattern;
  -- end record;

  -- Check and store a Rule
  procedure Store (Rule : Rule_Rec) is
  begin
    if not Sys_Calls.File_Check (Rule.File.Image) then
      raise File_Not_Found;
    end if;
    Rules.Rewind (Rules_Mng.Prev, Check_Empty => False);
    Rules.Insert (Rule);
  end Store;

  -- Get the number of stored Rules
  function Get_Number return Long_Longs.Ll_Natural is
    (Long_Longs.Ll_Natural (Rules.List_Length));

  -- Retrieve a Rule
  function Get_Rule (Number : in Long_Longs.Ll_Positive) return Rule_Rec is
    use type Long_Longs.Llu_Natural;
  begin
    if Long_Longs.Llu_Positive (Number) > Rules.List_Length then
      raise No_Rule;
    end if;
    -- Read the Rule
    Rules.Move_At (Long_Longs.Llu_Positive (Number));
    return Rule : Rule_Rec do
      Rules.Read (Rule, Rules_Mng.Current);
    end return;
  end Get_Rule;

end Rules;

