with Unchecked_Deallocation;
with Dynamic_List;
separate (Pattern)
package body Storage is
  procedure Free is new Unchecked_Deallocation (String, Str_Access);

  -- Patterns and their Terms are in order in the list.
  -- Rules are mixed.
  package Term_List_Mng is new Dynamic_List (Term_Rec);
  Term_List : Term_List_Mng.List_Type;

  -- Search first term of pattern in rule
  function Pattern_Match (Curr, Crit : Term_Rec) return Boolean is
  begin
    return Curr.Rule = Crit.Rule and then Curr.Id = Crit.Id;
  end Pattern_Match;
  procedure Search_Pattern is new Term_List_Mng.Search (Pattern_Match);

  -- Search first term of next pattern of same rule
  function Pattern_After (Curr, Crit : Term_Rec) return Boolean is
  begin
    return Curr.Rule = Crit.Rule and then Curr.Id > Crit.Id;
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
  begin
    for I in Rule_No'Range loop
      Term.Rule := I;
      begin
        Search_Rule (Term_List, Term, From => Term_List_Mng.Absolute);
      exception
        when Term_List_Mng.Not_In_List =>
          -- This rule does not exist;
          return I;
      end;
    end loop;
    -- All rules are used!
    raise No_Rule;
  end Get_Free_Rule;

  -- Local utility
  function Del_Term return Boolean is
  begin
    if Term_List_Mng.Get_Position (Term_List)
    /= Term_List_Mng.List_Length (Term_List) then
      Term_List_Mng.Delete (Term_List);
      return False;
    else 
      Term_List_Mng.Delete (Term_List, Term_List_Mng.Prev);
      return True;
    end if;
  end Del_Term;

  -- Delete all terms of all aptterns of a rule
  procedure Del_Rule (Rule : in Rule_No) is

    function Find_Rule (From_Current : Boolean) return Boolean is
      Term : Term_Rec;
    begin
      Term.Rule := Rule;
      if From_Current then
        Search_Rule (Term_List, Term, From => Term_List_Mng.From_Current);
      else
        Search_Rule (Term_List, Term, From => Term_List_Mng.Absolute);
      end if;
      return True;
    exception
      when Term_List_Mng.Not_In_List =>
        -- This rule does not exist
        return False;
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
  function Pattern_Exists (Rule : in Rule_No; Id : Pattern_Id) return Boolean is
    Term : Term_Rec;
  begin
    -- Search this pattern id
    begin
      Term.Rule := Rule;
      Term.Id := Id;
      Search_Pattern (Term_List, Term, From => Term_List_Mng.Absolute);
    exception
      when Term_List_Mng.Not_In_List =>
        return False;
    end;
    return True;
  end Pattern_Exists;

  -- Delete all terms of current pattern
  procedure Delete_Current_Pattern is
    Rule : Rule_No;
    Id : Pattern_Id;
    Term : Term_Rec;
  begin
    if Term_List_Mng.Is_Empty (Term_List) then
      raise Invalid_Pattern;
    end if;
    -- Get current pattern id
    Term_List_Mng.Read (Term_List, Term, Term_List_Mng.Current);
    Rule := Term.Rule;
    Id := Term.Id;
    -- Remove all records with same id
    loop
      Free (Term.Str_Acc);
      exit when Del_Term;
      Term_List_Mng.Read (Term_List, Term, Term_List_Mng.Current);
      -- New pattern of same rule
      exit when Term.Rule = Rule and then Term.Id /= Id;
    end loop;
  end Delete_Current_Pattern;

  -- Append terms to a new pattern
  In_Prev : Boolean := True;
  Term_Rule : Rule_No := Rule_No'First;
  Term_Id : Pattern_Id := Pattern_Id'First;
  Pat_Cb  : Match_Cb_Access;
  procedure Create_Pattern (Rule : in Rule_No;
                            Id : in Pattern_Id;
                            Cb : in Match_Cb_Access) is
    Term : Term_Rec;
  begin
    -- Move to end of previous pattern if posssible
    In_Prev := True;
    if not Term_List_Mng.Is_Empty (Term_List) then
      begin
        Term.Rule := Rule;
        Term.Id := Id;
        Next_Pattern (Term_List, Term, From => Term_List_Mng.Absolute);
        -- Found a following pattern of this rule
        if Term_List_Mng.Get_Position (Term_List) /= 1 then
          -- We can move to prev cell, so next term can appended
          Term_List_Mng.Move_To (Term_List, Term_List_Mng.Prev);
        else
          -- We are at first pos but in following pattern
          --  next term will need to be prepended
          In_Prev := False;
        end if;
      exception
        when Term_List_Mng.Not_In_List =>
          -- No following pattern, append to list
          Term_List_Mng.Move_To (Term_List, Term_List_Mng.Prev, 0, False);
          In_Prev := True;
      end;
    end if;
    Term_Rule := Rule;
    Term_Id := Id;
    Pat_Cb := Cb;
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
    Term.Optio := Optio;
    Term.Repet := Repet;
    Term.Cb := Pat_Cb;
    if In_Prev then
      Term_List_Mng.Insert (Term_List, Term, Term_List_Mng.Next);
    else
      -- Could move before insertion at creation
      Term_List_Mng.Insert (Term_List, Term, Term_List_Mng.Prev);
      In_Prev := False;
    end if;
  end Add_Term;

  -- Get terms one by one
  The_End : Boolean := False;
  The_Rule : Rule_No := Rule_No'First;
  procedure Rewind (Rule : Rule_No) is
  begin                 
    if not Term_List_Mng.Is_Empty (Term_List) then
      Term_List_Mng.Move_To (Term_List, Term_List_Mng.Next, 0, False);
      The_End := False;
    else
      The_End := True;
    end if;
    The_Rule := Rule;
  end Rewind;

  function Next_Term (Lower_Case : Boolean) return Term_Rec is
    No_Term, Term : Term_Rec;
  begin
    if The_End then
      return No_Term;
    end if;

    loop
      Term_List_Mng.Read (Term_List, Term, Term_List_Mng.Current);
      -- Move to next or this is the end
      if Term_List_Mng.Get_Position (Term_List)
      /= Term_List_Mng.List_Length (Term_List) then
        Term_List_Mng.Move_To (Term_List);
      else
        The_End := True;
      end if;
      -- Term got is of correct rule
      if Term.Rule = The_Rule then
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

