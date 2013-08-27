-- Set la list of regex and success condition
-- Check a string versus le citeria one after the other
with Unchecked_Deallocation;
package body Regex_Filters is

  procedure Free is new Unchecked_Deallocation (
                Regular_Expressions.Compiled_Pattern,
                Pattern_Access);

  procedure Free (Cell : in out Filter_Cell) is
  begin
    Regular_Expressions.Free(Cell.Pattern.all);
    Free(Cell.Pattern);
  end Free;

  -- Append the regex Criteria and the success condition Match to the Filter
  procedure Add_Regex (Filter : in out Regex_Filter;
            Criteria : in String;
            Match : in Boolean) is
    Cell : Filter_Cell;
    Ok : Boolean;
  begin
    -- Create/init cell
    Cell.Match := Match;
    Cell.Pattern := new Regular_Expressions.Compiled_Pattern;
    -- Compile expression
    Regular_Expressions.Compile (Cell.Pattern.all, Ok, Criteria);
    if Ok then
      -- Insert if Ok
      Filter.List.Insert(Cell);
    else
      -- Roll back, free regex pattern
      Free (Cell);
      raise Invalid_Regex;
    end if;
  end Add_Regex;

  -- Check Str versus first Criteria.
  -- Success is if it matches and then Match was set for this Criteria,
  --  or if does not match and Match was not set.
  -- If success, then go to next criteria
  -- Return True is success for all criterias
  function Check (Str : in String;
                  Filter : in out Regex_Filter;
                  Go_On_Success : Boolean := True) return Boolean is
    Loc_List : Filter_List_Mng.List_Type;
    Success : Boolean;
    Remains : Boolean;
    Cell : Filter_Cell;
    N_Match : Natural;
    Match : Boolean;
  begin
    -- True if empty list
    if Filter.List.Is_Empty then
      return True;
    end if;
    -- Make a copy of list container, just for scanning
    Loc_List.Unchecked_Assign(Filter.List);
    -- Rewind
    Loc_List.Move_To(Number => 0, From_Current => False);

    -- Loop of successive tests
    loop
      Remains := Loc_List.Check_Move;
      -- Read criteria
      if Remains then
        Loc_List.Read(Cell);
      else
        Loc_List.Read(Cell, Filter_List_Mng.Current);
      end if;
      -- Check regex and see if it must match
      Regular_Expressions.Exec(Cell.Pattern.all, Str, N_Match,
                               Regular_Expressions.No_Match_Array);
      Match := N_Match = 1;
      Success := Match = Cell.Match;
      -- Done if Success and then not Go_On_Success
      --  or else not Success and then Go_On_Success
      --  or else no more criteria
      exit when Success /= Go_On_Success or else not Remains;
    end loop;
    return Success;
  end Check;

  procedure Clear_Filter (Filter : in out Regex_Filter) is
    Cell : Filter_Cell;
  begin
    -- Done if empty list
    if Filter.List.Is_Empty then
      return;
    end if;

    -- Rewind
    Filter.List.Rewind;
    -- Loop of Get
    loop
      Cell := Filter.List.Get;
      Free (Cell);
      exit when Filter.List.Is_Empty;
    end loop;
  end Clear_Filter;

  overriding procedure Finalize (Filter : in out Regex_Filter) is
  begin
    Clear_Filter (Filter);
  end Finalize;

end Regex_Filters;

