with Database;
package body Analist is

  -- For sorting result
  -- Longer words first, then in alphabetical
  function Less_Than (El1, El2 : As.U.Asu_Us) return Boolean is
    L1, L2 : Natural;
  begin
    L1 := El1.Length;
    L2 := El2.Length;
    if L1 = L2 then
      return El1.Image < El2.Image;
    else
      -- Longer words come first
      return L1 > L2;
    end if;
  end Less_Than;
  procedure Sort is new As.U.Utils.Asu_Dyn_List_Mng.Sort (Less_Than);

  -- Search the anagrams of a given Length among Letters
  procedure Try (Db : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type;
                 Dlist : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type;
                 Letters : in String) is
    Moved : Boolean;
    Uword : As.U.Asu_Us;
    Word_Valid, Letter_Valid : Boolean;
    Used : array (1 .. Letters'Length) of Boolean;
  begin
    if Db.Is_Empty then
      return;
    end if;
    -- Iterate on all words
    Db.Rewind;
    loop
      -- Get the Word
      Db.Read (Uword, Moved => Moved);
      declare
        Word : constant String := Uword.Image;
      begin
        -- Process word only if its length  is compatible
        if Word'Length <= Letters'Length then
          -- Init for this Word
          Word_Valid := True;
          Used := (others => False);

          -- See if each letter of Word exists in Letters
          for W of Word loop
            Letter_Valid := False;
            for J in Letters'Range loop
              if W = Letters(J) and then not Used(J) then
                -- This letter is in Letters and not used yet => valid
                Used(J) := True;
                Letter_Valid := True;
                exit;
              end if;
            end loop;
            if not Letter_Valid then
              Word_Valid := False;
              exit;
            end if;
          end loop;

          if Word_Valid then
            -- Insert valid word in dynamic list
            Dlist.Insert (Uword);
          end if;

        end if;
      end;

      exit when not Moved;
    end loop;
  end Try;

  -- List the anagrams of Letters in the database
  procedure List (Letters : in String;
                  In_Nouns : in Boolean;
                  Anagrams : out As.U.Utils.Asu_Ua.Unb_Array) is

    -- Dynamic list of words found
    Dlist : As.U.Utils.Asu_Dyn_List_Mng.List_Type;
    -- Previous and current word
    Prev, Curr : As.U.Asu_Us;
    Moved : Boolean;
    use type As.U.Asu_Us;
  begin
    Anagrams.Set_Null;
    if Letters'Length > Max_Len then
      raise Too_Long;
    end if;
    if Letters = "" then
      return;
    end if;

    -- Search various anagram lengths
    if not In_Nouns then
      Try (Database.Words_Db, Dlist, Letters);
    else
      Try (Database.Nouns_Db, Dlist, Letters);
    end if;

    -- Sort result
    Sort (Dlist);

    -- Copy List to array, Remove duplicates
    if not Dlist.Is_Empty then
      Dlist.Rewind;
      Dlist.Read (Prev, Moved => Moved);
      Anagrams.Append (Prev);
      if Moved then
        loop
          Dlist.Read (Curr, Moved => Moved);
          if Curr /= Prev then
            Anagrams.Append (Curr);
            Prev := Curr;
          end if;
          exit when not Moved;
        end loop;
      end if;
    end if;

  end List;

end Analist;

