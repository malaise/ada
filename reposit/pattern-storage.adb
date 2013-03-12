with Unchecked_Deallocation;
with Dynamic_List;
separate (Pattern)
package body Storage is
  procedure Free is new Unchecked_Deallocation (String, Str_Access);

  -- Patterns and their Terms are in order in the list.
  -- Rules are mixed.
  package Term_Dyn_List_Mng is new Dynamic_List (Term_Rec);
  package Term_List_Mng renames Term_Dyn_List_Mng.Dyn_List;
  Term_List : Term_List_Mng.List_Type;

  -- Search first term of pattern in rule
  function Pattern_Match (Curr, Crit : Term_Rec) return Boolean is
  begin
    return Curr.Rule = Crit.Rule
           and then Curr.Str_Acc /= null
           and then Curr.Id = Crit.Id;
  end Pattern_Match;
  procedure Search_Pattern is new Term_List_Mng.Search (Pattern_Match);

  -- Search first term of next pattern of same rule
  function Pattern_After (Curr, Crit : Term_Rec) return Boolean is
  begin
    return Curr.Rule = Crit.Rule
           and then Curr.Str_Acc /= null
           and then Curr.Id > Crit.Id;
  end Pattern_After;
  procedure Next_Pattern is new Term_List_Mng.Search (Pattern_After);

  -- Search first term of first pattern of rule
  function Rule_Match (Curr, Crit : Term_Rec) return Boolean is
  begin
    return Curr.Rule = Crit.Rule;
  end Rule_Match;
  procedure Search_Rule is new Term_List_Mng.Search (Rule_Match);

  -- Return an unused rule
  function Get_Free_Rule return Rule_No is
    Term : Term_Rec;
    Found : Boolean;
  begin
    for I in Valid_Rule_No_Range loop
      Term.Rule := (No => I);
      Search_Rule (Term_List, Found, Term, From => Term_List_Mng.Absolute);
      if not Found then
        -- This rule does not exist: insert a dummy term
        Term_List.Insert (Term);
        return (No => I);
      end if;
    end loop;
    -- All rules are used!
    raise No_Rule;
  end Get_Free_Rule;

  -- Local utility
  function Del_Term return Boolean is
    Moved : Boolean;
  begin
    Term_List.Delete (Moved => Moved);
    return not Moved;
  end Del_Term;

  -- Delete all terms of all patterns of a rule
  procedure Del_Rule (Rule : in Rule_No) is

    function Find_Rule (From_Current : Boolean) return Boolean is
      Term : Term_Rec;
      Found : Boolean;
    begin
      Term.Rule := Rule;
      Search_Rule (Term_List, Found, Term,
          From => (if From_Current then Term_List_Mng.From_Current
                   else Term_List_Mng.Absolute));
      return Found;
    end Find_Rule;

  begin
    if not Find_Rule (False) then
      return;
    end if;
    loop
      exit when Del_Term;
      exit when not Find_Rule (True);
    end loop;
  end Del_Rule;


  -- Check if pattern Id exists
  -- For usage Next_Term
  The_End : Boolean := False;
  The_Rule : Rule_No := No_Rule_No;
  function Pattern_Exists (Rule : in Rule_No; Id : Pattern_Id) return Boolean is
    Term : Term_Rec;
    Found : Boolean;
  begin
    -- Search this pattern id
    Term.Rule := Rule;
    Term.Id := Id;
    Search_Pattern (Term_List, Found, Term, From => Term_List_Mng.Absolute);
    The_End := not Found;
    The_Rule := Rule;
    return Found;
  end Pattern_Exists;

  -- Delete all terms of current pattern
  procedure Delete_Current_Pattern is
    Term : Term_Rec;
    Found : Boolean;
  begin
    if Term_List.Is_Empty then
      raise Invalid_Pattern;
    end if;
    -- Get current pattern id
    Term_List.Read (Term, Term_List_Mng.Current);
    -- Remove all records with same id
    loop
      -- Delete current and exit if end of list
      Free (Term.Str_Acc);
      exit when Del_Term;
      -- Search next term of pattern from current and exit if no more
      Search_Pattern (Term_List, Found, Term, From => Term_List_Mng.From_Current);
      exit when not Found;
      Term_List.Read (Term, Term_List_Mng.Current);
    end loop;
  end Delete_Current_Pattern;

  -- Append terms to a new pattern
  In_Prev    : Boolean := True;
  Term_Rule  : Rule_No := No_Rule_No;
  Term_Id    : Pattern_Id := Pattern_Id'First;
  Term_Id4Cb : Pattern_Id := Pattern_Id'First;
  Term_Cb    : Match_Cb_Access := null;
  procedure Create_Pattern (Rule  : in Rule_No;
                            Id    : in Pattern_Id;
                            Cb    : in Match_Cb_Access;
                            Id4Cb : in Pattern_Id) is
    Term : Term_Rec;
    Found : Boolean;
  begin
    -- Move to end of previous pattern if posssible
    In_Prev := True;
    if not Term_List.Is_Empty then
      Term.Rule := Rule;
      Term.Id := Id;
      Next_Pattern (Term_List, Found, Term, From => Term_List_Mng.Absolute);
      if Found then
        -- Found a following pattern of this rule
        if Term_List.Check_Move (Term_List_Mng.Prev) then
          -- We can move to prev cell, so next term can appended
          Term_List.Move_To (Term_List_Mng.Prev);
        else
          -- We are at first pos but in following pattern
          --  next term will need to be prepended
          In_Prev := False;
        end if;
      else
        -- No following pattern, append to list
        Term_List.Rewind (True, Term_List_Mng.Prev);
        In_Prev := True;
      end if;
    end if;
    Term_Rule := Rule;
    Term_Id := Id;
    Term_Id4Cb := Id4Cb;
    Term_Cb := Cb;
  end Create_Pattern;

  procedure Add_Term (Str : in String;
                      Optio : in Boolean;
                      Repet : in Boolean) is
    Term : Term_Rec;
  begin
    -- Insert new term
    Term.Str_Acc := new String'(Str);
    Term.Rule := Term_Rule;
    Term.Id := Term_Id;
    Term.Id4Cb := Term_Id4Cb;
    Term.Optio := Optio;
    Term.Repet := Repet;
    Term.Cb := Term_Cb;
    Term_List.Insert (Term, (if In_Prev then Term_List_Mng.Next
                             -- Could move before insertion at creation
                             else Term_List_Mng.Prev));
  end Add_Term;

  -- Get terms one by one
  procedure Rewind (Rule : Rule_No) is
  begin
    Term_List.Rewind (False);
    The_End := Term_List.Is_Empty;
    The_Rule := Rule;
  end Rewind;

  function Next_Term (Lower_Case : Boolean) return Term_Rec is
    No_Term, Term : Term_Rec;
  begin
    if The_End then
      return No_Term;
    end if;

    loop
      Term_List.Read (Term, Term_List_Mng.Current);
      -- Move to next or this is the end
      if Term_List.Check_Move then
        Term_List.Move_To;
      else
        The_End := True;
      end if;
      -- Term got is of correct rule and not rule lock
      if Term.Rule = The_Rule and then Term.Str_Acc /= null then
        if Lower_Case then
          Term.Str_Acc.all := Lower_Str (Term.Str_Acc.all);
        end if;
        return Term;
      elsif The_End then
        return No_Term;
      end if;
    end loop;
  end Next_Term;

end Storage;

