with Ada.Text_Io;
with Parser, Environ, Lower_Str;
package body Pattern is

  Inited : Boolean := False;
  Debug : Boolean := False;

  procedure Init is
  begin
    if Inited then
      return;
    end if;
    Debug := Environ.Is_Yes ("PATTERN_DEBUG");
    Inited := True;
  end Init;

  procedure Put_Debug (Proc: in String; Msg : in String) is
  begin
    if not Debug then
      return;
    end if;
    Ada.Text_Io.Put_Line ("Pattern." & Proc &": " & Msg & ".");
  end Put_Debug;

  procedure Check_Rule (Rule : in Rule_No) is
  begin
    if Rule = No_Rule_No then
      raise No_Rule;
    end if;
  end Check_Rule;

  package Storage is

    -- Return an unused rule
    function Get_Free_Rule return Rule_No;

    -- Delete all terms of all patterns of a rule
    procedure Del_Rule (Rule : in Rule_No);


    -- Check if pattern Id exists and set to current
    function Pattern_Exists (Rule : Rule_No; Id : Pattern_Id) return Boolean;

    -- Delete all terms of current pattern
    procedure Delete_Current_Pattern;


    -- Append a terms to new pattern
    procedure Create_Pattern (Rule  : in Rule_No;
                              Id    : in Pattern_Id;
                              Cb    : in Match_Cb_Access;
                              Id4Cb : in Pattern_Id);
    procedure Add_Term (Str : in String;
                        Optio : in Boolean;
                        Repet : in Boolean);
                      

    -- Get terms of patterns of rule, one by one

    type Str_Access is access String;

    -- A record with these values (Str_Acc=null) is used
    --  internally to mark a used rule
    --  or returned by Next_Term when no more pattern for the rule
    type Term_Rec is record
      Rule    : Rule_No := No_Rule_No;
      Id      : Pattern_Id := Pattern_Id'First;
      Id4Cb   : Pattern_Id := Pattern_Id'First;
      Str_Acc : Str_Access := null;
      Optio   : Boolean := False;
      Repet   : Boolean := False;
      Cb      : Match_Cb_Access;
    end record;

    procedure Rewind (Rule : in Rule_No);
    function Next_Term (Lower_Case : Boolean) return Term_Rec;

  end Storage;
    

  package body Storage is separate;


  -- Separator for parsing patterns
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = ' ' or else C = Ascii.Ht;
  end Is_Sep;

  -- (Re)define a pattern
  -- May raise Invalid_Pattern if Pattern is not valid
  procedure Set (Rule     : in Rule_No;
                 Id       : in Pattern_Id;
                 Pattern  : in String;
                 Match_Cb : in Match_Cb_Access;
                 Id4Cb    : in Pattern_Id4Cb := Same_Id) is
    Iter : Parser.Iterator;
    First : Boolean;
    Optio : Boolean;
    Repet : Boolean;
    Termi : Boolean;
    Ok    : Boolean;
                      

  begin
    Init;
    Check_Rule (Rule);
    if Storage.Pattern_Exists (Rule, Id) then
      Storage.Delete_Current_Pattern;
    end if;
    -- Init
    Put_Debug ("Set", "Create patern to rule " & Image (Rule) &
                      ", id " & Id'Img);
    if Id4Cb = Same_Id then
      Storage.Create_Pattern (Rule, Id, Match_Cb, Id);
    else
      Storage.Create_Pattern (Rule, Id, Match_Cb, Id4Cb);
    end if;
    Parser.Set (Iter, Pattern, Is_Sep'Access);
    First := True;
    Optio := False;
    Repet := False;
    Termi := False;
    Ok := False;

    -- Parse string
    One_Word:
    loop
      declare
        Str : constant String := Parser.Next_Word (Iter);
      begin

        -- Check for end of parsing
        if Str = "" then
          if First then
            -- Empty pattern
            Put_Debug ("Set", "Add empty patern to rule " & Image (Rule)
                            & ", id " & Id'Img);
            Storage.Add_Term (Str, Optio, Repet);
            Ok := True;
          else
            -- End of pattern
            -- Check that no [ { left open (Termi should not be set)
            Ok := not (Optio or else Repet or else Termi);
          end if;
          exit One_Word;
        end if;
        First := False;

        -- In the middle of parsing
        if Str = "[" then
          -- [ shall start pattern
          if Termi or else Optio or else Repet then
            Ok := False;
            exit One_Word;
          else
            Optio := True;
          end if;
        elsif Str = "{" then
          -- { or [ { shall start pattern
          if Termi or else Repet then
            Ok := False;
            exit One_Word;
          else
            Repet := True;
          end if;
        elsif Str = "}" then
          -- } or } ] end pattern
          if not Termi or else not Repet then
            Ok := False;
            exit One_Word;
          else
            Repet := False;
            if not Optio then
              -- End of term
              Termi := False;
            end if;
          end if;
        elsif Str = "]" then
          -- ] end pattern
          if not Termi or else not Optio or else Repet then
            Ok := False;
            exit One_Word;
          else
            Optio := False;
          end if;
          -- End of term
          Termi := False;
        else
          -- Got a term
          if Termi then
            -- Term term
            Ok := False;
            exit One_Word;
          else
            -- Valid
            Put_Debug ("Set", "Add patern " & Str
                            & " " & Optio'Img & " " & Repet'Img
                            & " to rule " & Image (Rule)
                            & ", id "  & Id'Img);
            Storage.Add_Term (Str, Optio, Repet);
            Termi := Optio or else Repet;
          end if;
        end if;
      end;
    end loop One_Word;

    -- Cleanup and verdict
    Parser.Del (Iter);
    if not Ok then
      Del (Rule, Id);
      raise Invalid_Pattern;
    end if;
  end Set;


  -- Delete a pattern
  -- May raise Invalid_Pattern if the Id is not set
  procedure Del (Rule : in Rule_No; Id : in Pattern_Id) is
  begin
    Init;
    Check_Rule (Rule);
    if Storage.Pattern_Exists (Rule, Id) then
      Put_Debug ("Del", "Delete pattern rule " & Image (Rule)
                      & ", id " & Id'Img);
      Storage.Delete_Current_Pattern;
    else
      raise Invalid_Pattern;
    end if;
  end Del;


  -- Check Str versus patterns in crescent order if Ids
  -- Separators may be spaces or tabs.
  function Check (Rule : Rule_No;
                  Str : String;
                  Case_Sensitive : Boolean := True) return Boolean is
    Iter : Parser.Iterator;
    Ok : Boolean;
    Nb_Match : Natural;
    Term, Prev_Term : Storage.Term_Rec;
    Repeating : Boolean;
    Index : Natural;

    use type Storage.Str_Access;

    -- Get Parser.Current_Word, in lower case or not
    function Current_Word return String is
    begin
      if Case_Sensitive then
        return Parser.Current_Word (Iter);
      else
        return Lower_Str (Parser.Current_Word (Iter));
      end if;
    end Current_Word;

    -- Pull one term
    function Pull return Boolean is
    begin
      Prev_Term := Term;
      Term := Storage.Next_Term (not Case_Sensitive);
      -- No more term?
      if Term.Str_Acc = null then
        Put_Debug ("Check.Pull", "No more pattern");
        Term := Prev_Term;
        return True;
      end if;
      -- New pattern and OK?
      if Term.Id /= Prev_Term.Id and then Ok then
        Put_Debug ("Check", "End of pattern and OK");
        Term := Prev_Term;
        return True;
      end if;
      return False;
    end Pull;

    -- Remove terms until end of list, new pattern,
    --  or (if Only_Optio is set) until not an option
    function Flush (Only_Optio : Boolean) return Boolean is
    begin
      loop
        Prev_Term := Term;
        Term := Storage.Next_Term (not Case_Sensitive);
        if Term.Str_Acc = null then
          -- No more term
          Put_Debug ("Check.Flush", "No more term");
          return True;
        end if;
        if Term.Id /= Prev_Term.Id then
          -- New pattern
          Put_Debug ("Check.Flush", "New pattern");
          return False;
        end if;
        if Only_Optio and then not Term.Optio then
          -- Not an option
          Put_Debug ("Check.Flush", "Not and option");
          return False;
        end if;
        Put_Debug ("Check.Flush", "Flushing term " & Term.Str_Acc.all);
      end loop;
    end Flush;

    -- Cleanup and verdict. Return True if OK and Cb returned True.
    function Conclude return Boolean is
    Res : Boolean := False;
    begin
      if Term.Cb = null then
        Put_Debug ("Check", "Cb is null");
      end if;
      if Ok and then Term.Cb /= null then
        Put_Debug ("Check", "Calling Cb");
        Res := Term.Cb (Rule, Term.Id4Cb, Nb_Match, Iter);
      end if;
      Parser.Del (Iter);
      Put_Debug ("Check", "Done");
      return Res;
    end Conclude;

  begin
    Init;
    Check_Rule (Rule);
    Put_Debug ("Check", "Checking string " & Str
             & " in rule no " & Image (Rule));

    -- Init check
    if Case_Sensitive then
      Parser.Set (Iter, Str, Is_Sep'Access);
    else
      Parser.Set (Iter, Lower_Str (Str), Is_Sep'Access);
    end if;
    Storage.Rewind (Rule);
    Nb_Match := 0;
    Ok := False;
    Index := 0;
    Repeating := False;

    -- Get first term and word
    Prev_Term := Term;
    Term := Storage.Next_Term (not Case_Sensitive);
    Parser.Next_Word (Iter);

    -- No pattern
    if Term.Str_Acc = null then
      Put_Debug ("Check", "No pattern for rule");
      return False;
    end if;

    -- Empty String: Look for empty pattern
    if Parser.Current_Word (Iter) = "" then
      Put_Debug ("Check", "Empty input string");
      Ok := False;
      loop
        if Term.Str_Acc.all = "" then
          Put_Debug ("Check", "  Found wildcard term");
          Parser.Reset (Iter);
          Ok := True;
          exit;
        else
          Term := Storage.Next_Term (not Case_Sensitive);
          exit when Term.Str_Acc = null;
        end if;
      end loop;
      return Conclude;
    end if;

    -- Compare words of Str to current pattern
    One_Word:
    loop
      declare
        -- In lower case or not
        Strl : constant String := Current_Word;
      begin
        -- Match?
        Put_Debug ("Check", "Comparing word " & Strl
                          & " and term " & Term.Str_Acc.all
                          & " of pattern id " & Term.Id'Img);
        if Term.Str_Acc.all = "" then
          Put_Debug ("Check", "  Term is wildcard");
          Nb_Match := 0;
          Index := 0;
          Ok := True;
          exit One_Word;
        elsif Strl = "" then
          if Term.Optio then
            Put_Debug ("Check", "  End of string, flushing options");
            -- Flush all remaining options
            if Flush (True) or else Term.Id /= Prev_Term.Id then
              Put_Debug ("Check", "  All was options");
              Term := Prev_Term;
              exit One_Word;
            end if;
          end if;
          -- Reset Str and move to next pattern
          Put_Debug ("Check", "  Resetting for pattern no " & Image (Term.Rule)
                            & ", id " & Term.Id'Img);
          Parser.Reset (Iter);
          Parser.Next_Word (Iter);
          Nb_Match := 0;
          Index := 0;
          Ok := False;
          Repeating := False;
          exit One_Word when Flush (False);
        elsif Strl = Term.Str_Acc.all then
          Put_Debug ("Check", "  Match");
          Nb_Match := Nb_Match + 1;
          Index := Parser.Last_Index (Iter, False);
          Ok := True;
          -- Next str may mismatch with current term and still Ok
          Repeating := Term.Repet;
          -- Pop one word, and one term if not repetitive
          Parser.Next_Word (Iter);
          if not Term.Repet then
            exit One_Word when Pull;
          end if;
        else
          if not Term.Optio and then not Repeating then
            -- Mismatch, reset string and pull all terms of pattern
            Put_Debug ("Check", "  Mismatch. Resetting from pattern no "
                              & Image (Term.Rule) & ", id " & Term.Id'Img);
            Parser.Reset (Iter);
            Parser.Next_Word (Iter);
            Nb_Match := 0;
            Index := 0;
            Ok := False;
            exit One_Word when Flush (False);
          else
            -- Pull mismatching term if optionnal or end of repetition
            Put_Debug ("Check", "  Mismatch with option or end of repetition");
            exit One_Word when Pull;
            Repeating := False;
          end if;
        end if;

      end;

    end loop One_Word;

    -- Cleanup and verdict
    return Conclude;
  end Check;

  procedure Check (Rule : in Rule_No;
                   Str  : in String;
                   Case_Sensitive : in Boolean := True) is
    Dummy : Boolean;
  begin
    Dummy := Check (Rule, Str);
  end Check;


  -- Rule image
  function Image (Rule : Rule_No) return String is
  begin
    return Rule.No'Img;
  end Image;

  -- May raise No_Rule if no rule is available.
  function Get_Free_Rule return Rule_No is
    Rule : Rule_No;
  begin
    Init;
    Rule := Storage.Get_Free_Rule;
    Put_Debug ("Get_Free_Rule", "Allocating " & Image (Rule));
    return Rule;
  end Get_Free_Rule;

  -- Delete all patterns of a rule
  procedure Del_Rule (Rule : in Rule_No) is
  begin
    Init;
    Check_Rule (Rule);
    Put_Debug ("Del_Rule", "Deleting " & Image (Rule));
    Storage.Del_Rule (Rule);
  end Del_Rule;

end Pattern;


