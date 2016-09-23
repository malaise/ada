with Sys_Calls, Text_Line;
package body Analist is

  Words_Db, Nouns_Db : aliased As.U.Utils.Asu_Dyn_List_Mng.List_Type;
  type Db_Access is access all As.U.Utils.Asu_Dyn_List_Mng.List_Type;

  -- Local: init a data base
  procedure Init_Db (File_Name : in String;
                     Db : in out As.U.Utils.Asu_Dyn_List_Mng.List_Type) is
    Fd : Sys_Calls.File_Desc;
    File : Text_Line.File_Type;
  begin
    -- Clear
    Db.Delete_List;
    -- Load file
    Fd := Sys_Calls.Open (File_Name, Sys_Calls.In_File);
    File.Open (Text_Line.In_File, Fd);
    loop
      declare
        Word : constant String := File.Get;
        Len : Natural := Word'Length;
      begin
        -- End of file
        exit when Len = 0;
        if Word(Len) = Text_Line.Line_Feed_Char then
          Len := Len - 1;
        end if;
        -- Store word in Data_Base
        if Len < Max_Len then
          Db.Insert (As.U.Tus (Word(1 .. Len)));
        end if;
      end;
    end loop;

    -- Done
    File.Close;
    Sys_Calls.Close (Fd);

  exception
    when Sys_Calls.Name_Error | Sys_Calls.System_Error =>
      raise Init_Error;
  end Init_Db;

  -- Init database from a dictionary (file with one word per line)
  -- Reset it if already init
  procedure Init (Words_File, Nouns_File : in String) is
  begin
    Init_Db (Words_File, Words_Db);
    Init_Db (Nouns_File, Nouns_Db);
  end Init;

  -- Add a word in the database if does not exist
  procedure Add (Word : in As.U.Asu_Us; Noun : in Boolean) is
    Moved : Boolean;
    Uword : As.U.Asu_Us;
    Dba : Db_Access;
    use type As.U.Asu_Us;
  begin
    if Noun then
      Dba := Nouns_Db'Access;
    else
      Dba := Words_Db'Access;
    end if;
    -- Verify that word does not exist
    if not Dba.Is_Empty then
      -- Iterate on all words
      Dba.Rewind;
      loop
        -- Get the Word
        Dba.Read (Uword, Moved => Moved);
        if Word = Uword then
          -- This word already exists
          return;
        end if;
        exit when not Moved;
      end loop;
    end if;
    -- Insert word
    Dba.Insert (Word);
  end Add;

  -- Delete a word from the database if it exists
  procedure Del (Word : in As.U.Asu_Us; Noun : in Boolean) is
    Moved : Boolean;
    Uword : As.U.Asu_Us;
    Dba : Db_Access;
    use type As.U.Asu_Us;
  begin
    if Noun then
      Dba := Nouns_Db'Access;
    else
      Dba := Words_Db'Access;
    end if;

    -- Find the word if it exists
    if Dba.Is_Empty then
      return;
    end if;
    -- Iterate on all words
    Dba.Rewind;
    loop
      -- Get the word
      Dba.Read (Uword, Moved => Moved);
      if Word = Uword then
        -- The word exists
        Dba.Delete (Moved => Moved);
      end if;
      exit when not Moved;
    end loop;
  end Del;

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
      Try (Words_Db, Dlist, Letters);
    else
      Try (Nouns_Db, Dlist, Letters);
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

