with Unchecked_Deallocation;
with Dynamic_List;
separate (Pattern)
package body Storage is
  procedure Free is new Unchecked_Deallocation (String, Str_Access);
  --  type Str_Access is access String;

  --   type Term_Rec is record
  --     Id : Pattern_Id;
  --     No : Pattern_Id;
  --     Str_Acc : Str_Access;
  --     Repetitive : Boolean;
  --     Optionnal : Boolean;
  --   end Record;
  package Term_List_Mng is new Dynamic_List (Term_Rec);
  Term_List : Term_List_Mng.List_Type;

  -- Sort terms by pattern_id/term_no
  function Term_Before (El1, El2 : Term_Rec) return Boolean is
  begin
    return El1.Id < El2.Id
    or else (El1.Id = El2.Id and then El1.No < El2.No);
  end Term_Before;
  procedure Sort_Terms is new Term_List_Mng.Sort (Term_Before);

  -- Search first term of pattern
  function Pattern_Match (El1, El2 : Term_Rec) return Boolean is
  begin
    return El1.Id = El2.Id;
  end Pattern_Match;
  procedure Search_Pattern is new Term_List_Mng.Search (Pattern_Match);

  -- Search first term of next pattern
  function Pattern_After (El1, El2 : Term_Rec) return Boolean is
  begin
    return El1.Id > El2.Id;
  end Pattern_After;
  procedure Next_Pattern is new Term_List_Mng.Search (Pattern_After);



  -- Check if pattern Id exists
  function Pattern_Exists (Id : Pattern_Id) return Boolean is
    Term : Term_Rec;
  begin
    -- Search this pattern id
    begin
      Term.Id := Id;
      Search_Pattern (Term_List, Term, From_Current => False);
    exception
      when Term_List_Mng.Not_In_List =>
        return False;
    end;
    return True;
  end Pattern_Exists;

  -- Delete all terms of current pattern
  procedure Delete_Current_Pattern is
    Id : Pattern_Id;
    Term : Term_Rec;
  begin
    if Term_List_Mng.Is_Empty (Term_List) then
      raise Invalid_Pattern;
    end if;
    -- Get current pattern id
    Term_List_Mng.Read (Term_List, Term, Term_List_Mng.Current);
    Id := Term.Id;
    -- Remove all records with same id
    loop
      if Term_List_Mng.Get_Position (Term_List)
      /= Term_List_Mng.List_Length (Term_List) then
        Term_List_Mng.Delete (Term_List);
      else
        Term_List_Mng.Delete (Term_List, Term_List_Mng.Prev);
        exit;
      end if;
      Term_List_Mng.Read (Term_List, Term, Term_List_Mng.Current);
      exit when Term.Id /= Id;
    end loop;
  end Delete_Current_Pattern;

  -- Append a terms to a new pattern
  In_Prev : Boolean := True;
  Term_Id : Pattern_Id := Pattern_Id'First;
  Term_No : Pattern_Id := Pattern_Id'First;
  procedure Create_Pattern (Id : Pattern_Id) is
    Term : Term_Rec;
  begin
    -- Move to next pattern
    begin
      Term.Id := Id;
      Next_Pattern (Term_List, Term, From_Current => False);
    exception
      when Term_List_Mng.Not_In_List =>
        null;
    end;
    -- Try to move to end of prev pattern
    if Term_List_Mng.Get_Position (Term_List) /= 1 then
      Term_List_Mng.Move_To (Term_List, Term_List_Mng.Prev);
      In_Prev := True;
    else
      In_Prev := False;
    end if;
    Term_Id := Id;
    Term_No := Pattern_Id'First;
  end Create_Pattern;

  procedure Add_Term (Str : in String;
                      Optio : in Boolean;
                      Repet : in Boolean) is
    Term : Term_Rec;
  begin
    Term.Str_Acc := new String'(Str);
    Term.Id := Term_Id;
    Term.No := Term_No;
    Term.Optio := Optio;
    Term.Repet := Repet;
  end Add_Term;

  -- Get terms one by one
  The_End : Boolean := False;
  procedure Rewind is
  begin                 
    if not Term_List_Mng.Is_Empty (Term_List) then
      Term_List_Mng.Move_To (Term_List, Term_List_Mng.Next, 0, False);
      The_End := False;
    else
      The_End := True;
    end if;
  end Rewind;

  function Next_Term return Term_Rec is
    Term : Term_Rec;
  begin
    if The_End then
      return Term;
    end if;
    Term_List_Mng.Read (Term_List, Term, Term_List_Mng.Current);
    -- Move to next or this is the end
    if Term_List_Mng.Get_Position (Term_List)
    /= Term_List_Mng.List_Length (Term_List) then
      Term_List_Mng.Move_To (Term_List);
    else
      The_End := True;
    end if;
    return Term;
  end Next_Term;

end Storage;
