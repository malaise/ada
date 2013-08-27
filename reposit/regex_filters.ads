-- Set a list of regex expressions and success conditions
-- Check a string versus the citeria one after the other
with Ada.Finalization;
with Dynamic_List, Regular_Expressions;
package Regex_Filters is

  type Regex_Filter is tagged limited private;

  -- Append the regex Criteria and the success condition Match to the Filter
  Invalid_Regex : exception;
  procedure Add_Regex (Filter : in out Regex_Filter;
                       Criteria : in String;
                       Match : in Boolean);

  -- Check Str versus first Criteria.
  -- Success is if it matches and then Match is True for this Criteria,
  --  or if it does not match and Match is False for this Criteria.
  -- If success, if     Go_On_Success then go to next criteria else return True
  -- If failure, if not Go_On_Success then go to next criteria else return False
  -- Return result of last check, True if no criteria is defined
  function Check (Str : in String; Filter : in out Regex_Filter;
                  Go_On_Success : Boolean := True) return Boolean;

  -- Remove all criteria
  procedure Clear_Filter (Filter : in out Regex_Filter);

private
  type Pattern_Access is access Regular_Expressions.Compiled_Pattern;

  type Filter_Cell is record
    Pattern : Pattern_Access;
    Match : Boolean;
  end record;

  package Filter_Dyn_List_Mng is new Dynamic_List (Filter_Cell);
  package Filter_List_Mng renames Filter_Dyn_List_Mng.Dyn_List;

  type Regex_Filter is limited new Ada.Finalization.Limited_Controlled
                               with record
    List :  Filter_List_Mng.List_Type;
  end record;
  overriding procedure Finalize (Filter : in out Regex_Filter);

end Regex_Filters;

