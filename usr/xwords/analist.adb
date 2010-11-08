with Sys_Calls, Text_Line;
package body Analist is

  Data_Base : Asu_Dyn_List_Mng.List_Type;

  -- Init database from a dictionnary (file with one word per line)
  -- Reset it if already init
  procedure Init (File_Name : in String) is
    Fd : Sys_Calls.File_Desc;
    File : Text_Line.File_Type;
  begin
    -- Clear
    Data_Base.Delete_List;
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
          Data_Base.Insert (Asu_Tus (Word(1 .. Len)));
        end if;
      end;
    end loop;

    -- Done
    File.Close;
    Sys_Calls.Close (Fd);

  exception
    when Sys_Calls.Name_Error | Sys_Calls.System_Error =>
      raise Init_Error;
  end Init;

  -- For sorting result
  -- Longer words first, then in alphabetical
  function Less_Than (El1, El2 : Asu_Us) return Boolean is
    L1, L2 : Natural;
  begin
    L1 := Asu.Length (El1);
    L2 := Asu.Length (El2);
    if L1 = L2 then
      return Asu_Ts (El1) < Asu_Ts (El2);
    else
      -- Longer words come first
      return L1 > L2;
    end if;
  end Less_Than;
  procedure Sort is new Asu_Dyn_List_Mng.Sort (Less_Than);

  -- Search the anagrams of a given Length among Letters
  procedure Try (Dlist : in out Asu_Dyn_List_Mng.List_Type;
                 Letters : in String) is
    Moved : Boolean;
    Uword : Asu_Us;
    Word_Valid, Letter_Valid : Boolean;
    Used : array (1 .. Letters'Length) of Boolean;
  begin
    if Data_Base.Is_Empty then
      return;
    end if;
    -- Iterate on all words
    Data_Base.Rewind;
    loop
      -- Init for this Word
      Word_Valid := True;
      Used := (others => False);
      -- Get the Word
      Data_Base.Read (Uword, Moved => Moved);
      declare
        Word : constant String := Asu_Ts (Uword);
      begin
        -- Process word only if its length  is compatible
        if Word'Length <= Letters'Length then

          -- See if each letter of Word exists in Letters
          for I in Word'Range loop
            Letter_Valid := False;
            for J in Letters'Range loop
              if Word(I) = Letters(J) and then not Used(J) then
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
                  Anagrams : out Asu_Ua.Unb_Array) is

    -- Dynamic list of words found
    Dlist : Asu_Dyn_List_Mng.List_Type;
    -- Previous and current word
    Prev, Curr : Asu_Us;
    Moved : Boolean;
    use type Asu_Us;
  begin
    Anagrams := Asu_Ua.Null_Unb_Array;
    if Letters'Length > Max_Len then
      raise Too_Long;
    end if;
    if Letters = "" then
      return;
    end if;

    -- Search various anagram lengths
    Try (Dlist, Letters);

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

