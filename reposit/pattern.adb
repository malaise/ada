with Ada.Text_Io;
with Parser, Environ;
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


  package Storage is

    -- Check if pattern Id exists and set to current
    function Pattern_Exists (Rule : Rule_No; Id : Pattern_Id) return Boolean;

    -- Delete all terms of current pattern
    procedure Delete_Current_Pattern;

    -- Append a terms to new pattern
    procedure Create_Pattern (Rule : in Rule_No;
                              Id : in Pattern_Id;
                              Cb : in Match_Cb_Access);
    procedure Add_Term (Str : in String;
                        Optio : in Boolean;
                        Repet : in Boolean);
                      
    -- Get terms one by one
    procedure Rewind (Rule : in Rule_No);

    type Str_Access is access String;

    type Term_Rec is record
      Rule : Rule_No := Rule_No'First;
      Id : Pattern_Id := Pattern_Id'First;
      Str_Acc : Str_Access := null;
      Optio : Boolean := False;
      Repet : Boolean := False;
      Cb : Match_Cb_Access;
    end record;

    function Next_Term return Term_Rec;

  end Storage;
    

  package body Storage is separate;


  -- Separator for parsing patterns
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = ' ' or else C = Ascii.Ht;
  end Is_Sep;

  -- (Re)define a pattern
  -- May raise Invalid_Pattern if Pattern is not valid
  procedure Set (Rule : in Rule_No;
                 Id : in Pattern_Id;
                 Pattern  : in String;
                 Match_Cb : in Match_Cb_Access) is
    Iter : Parser.Iterator;
    First : Boolean;
    Optio : Boolean;
    Repet : Boolean;
    Termi : Boolean;
    Ok    : Boolean;
                      

  begin
    Init;
    if Storage.Pattern_Exists (Rule, Id) then
      Storage.Delete_Current_Pattern;
    end if;
    -- Init
    Put_Debug ("Set", "Create patern to rule " & Rule'Img & ", id " & Id'Img);
    Storage.Create_Pattern (Rule, Id, Match_Cb);
    Parser.Create (Pattern, Is_Sep'Access, Iter);
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
            Put_Debug ("Set", "Add empty patern to rule " & Rule'Img
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
                            & " to rule " & Rule'Img & ", id "  & Id'Img);
            Storage.Add_Term (Str, Optio, Repet);
            Termi := Optio or else Repet;
          end if;
        end if;
      end;
    end loop One_Word;

    -- Cleanup and verdict
    Parser.Delete (Iter);
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
    if Storage.Pattern_Exists (Rule, Id) then
      Put_Debug ("Del", "Delete pattern rule " & Rule'Img & ", id " & Id'Img);
      Storage.Delete_Current_Pattern;
    else
      raise Invalid_Pattern;
    end if;
  end Del;


  -- Check Str versus patterns in crescent order if Ids
  -- Separators may be spaces or tabs.
  function Check (Rule : Rule_No; Str : String) return Boolean is
    Iter : Parser.Iterator;
    First_Word : Boolean;
    Ok : Boolean;
    Nb_Match : Natural;
    Term, Prev_Term : Storage.Term_Rec;
    Pull : Boolean;
    Index : Natural;

    use type Storage.Str_Access;

    -- Remove terms until end of list, new pattern,
    --  or (if Only_Optio is set) until not an option
    function Flush (Only_Optio : Boolean) return Boolean is
    begin
      loop
        Prev_Term := Term;
        Term := Storage.Next_Term;
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

  begin
    Init;
    Put_Debug ("Check", "Checking string " & Str & " in rule no " & Rule'Img);
    -- Init check
    Parser.Create (Str, Is_Sep'Access, Iter);
    Storage.Rewind (Rule);
    First_Word := True;
    Nb_Match := 0;
    Ok := False;
    Term := Storage.Next_Term;
    Prev_Term := Term;
    Index := 0;

    -- No pattern
    if Term.Str_Acc = null then
      Put_Debug ("Check", "No pattern for rule");
      return False;
    end if;

    -- Compare words of Str to current pattern
    One_Word:
    loop
      declare
        Strl : constant String := Parser.Next_Word (Iter);
      begin
        -- Check for end of input string
        if Strl = "" then
          if First_Word then
            -- Empty String. Look for empty pattern
            Put_Debug ("Check", "Empty input string");
            Ok := False;
            loop
              if Term.Str_Acc.all = "" then
                Put_Debug ("Check", "  Found wildcard term");
                Parser.Reset (Iter);
                Ok := True;
                exit One_Word;
              end if;
              Term := Storage.Next_Term;
              exit when Term.Str_Acc = null;
            end loop;
            exit One_Word;
          end if;
        end if;
        First_Word := False;

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
          Put_Debug ("Check", "  End of string, flushing options");
          -- Flush all remaining options
          exit One_Word when Flush (True);
          if Term.Id /= Prev_Term.Id then
            -- All was option
            Put_Debug ("Check", "  All was options");
            Term := Prev_Term;
            exit One_Word;
          else
            Put_Debug ("Check", "  Resetting for pattern no " & Term.Rule'Img
                              & ", id " & Term.Id'Img);
            Parser.Reset (Iter);
            Nb_Match := 0;
            Index := 0;
            Ok := False;
            Pull := False;
          end if;
        elsif Strl = Term.Str_Acc.all then
          Put_Debug ("Check", "  Match");
          Nb_Match := Nb_Match + 1;
          Index := Parser.Last_Index (Iter);
          Ok := True;
          -- Pull term if not repetitive
          Pull := not Term.Repet;
        else
          Put_Debug ("Check", "  Mismatch");
          if not Term.Optio then
            -- Mismatch, reset string and pull all terms of pattern
            Put_Debug ("Check", "  Resetting from pattern no " & Term.Rule'Img
                              & ", id " & Term.Id'Img);
            Parser.Reset (Iter);
            Nb_Match := 0;
            Index := 0;
            Ok := False;
            Pull := False;
            exit One_Word when Flush (False);
          else
            -- Pull mismatching term if optionnal
            Pull := Term.Optio;
          end if;
        end if;

        if Pull then
          Prev_Term := Term;
          Term := Storage.Next_Term;
          -- No more term
          if Term.Str_Acc = null then
            Put_Debug ("Check", "No more pattern");
            Term := Prev_Term;
            exit One_Word;
          end if;

          -- New pattern and OK?
          if Term.Id /= Prev_Term.Id and then Ok then
            Put_Debug ("Check", "End of pattern and OK");
            Term := Prev_Term;
            exit One_Word;
          end if;
        end if;

      end;

    end loop One_Word;

    -- Cleanup and verdict
    if Ok and then Term.Cb /= null then
      Put_Debug ("Check", "Calling Cb");
      Term.Cb (Rule, Str, Term.Id, Nb_Match, Index);
    end if;
    Parser.Delete (Iter);
    Put_Debug ("Check", "Done.");
    return OK;
  end Check;

  procedure Check (Rule : in Rule_No; Str : in String) is
    Dummy : Boolean;
  begin
    Dummy := Check (Rule, Str);
  end Check;

end Pattern;


