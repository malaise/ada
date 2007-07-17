-- Set a list of regex expressions and success conditions
-- Check a string versus the citeria one after the other
with Dynamic_List, Regular_Expressions;
package Regex_Filters is

  type Regex_Filter is limited private;

  -- Append the regex Criteria and the success condition Match to the Filter
  Invalid_Regex : exception;
  procedure Add_Regex (Filter : in out Regex_Filter;
            Criteria : in String;
            Match : in Boolean);

  -- Check Str versus first Criteria.
  -- Success is if it matches and then Match was set for this Criteria,
  --  or if does not match and Match was not set.
  -- If success, then go to next criteria
  -- Return True is success for all criterias
  function Check (Str : String; Filter : in Regex_Filter) return Boolean;


  procedure Clear_Filter (Filter : in out Regex_Filter);

private
  type Pattern_Access is access Regular_Expressions.Compiled_Pattern;

  type Filter_Cell is record
    Pattern : Pattern_Access;
    Match : Boolean;
  end record;

  package Filter_Dyn_List_Mng is new Dynamic_List (Filter_Cell);
  package Filter_List_Mng renames Filter_Dyn_List_Mng.Dyn_List;

  type Regex_Filter is new Filter_List_Mng.List_Type;

end Regex_Filters;

