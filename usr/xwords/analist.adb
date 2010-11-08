-- with File_Hash;
package body Analist is
  -- Result : array (1 .. File_Hash.Max_Str_Len) of Asu_Ua.Unb_Array;

  -- The Ok function when Check is null
  function Check_Ok (Word : String) return Boolean is
  pragma Unreferenced (Word);
  begin
    return True;
  end Check_Ok;

  -- Put all permutations of Letters from Index to Last
  -- Then restart the analyse whith one letter less at any position
  procedure Permute (Word  : in String;
                     Check : in Check_Access;
                     Min   : in Positive;
                     Pivot : in Natural;
                     Length : in Natural;
                     Buffer : in out String) is
    New_Word : String (1 .. Word'Length);
    K : Positive;
    L : Natural;
    F : constant Natural := Buffer'First;
  begin
    if Pivot = 0 then
      return;
    end if;

    L := Length + 1;
    -- One tree for each letter
    for I in 1 .. Pivot loop
      -- Append a new letter
      Buffer(Buffer'Length - Pivot + 1) := Word(I);
      if Length >= Min and then Check (Buffer(F .. L)) then
        Result(Length).Append (Asu_Tus (Buffer(F .. L)));
      end if;
      -- Set new combination
      K := 1;
      for J in 1 .. Pivot loop
        if J /= I then
          New_Word(K) := Word(J);
          K := K + 1;
        end if;
      end loop;

      -- New tree
      Permute (New_Word, Check, Min, Pivot - 1, Length + 1, Buffer);
      
    end loop;

  end Permute;

  -- List the various combinations in Letters that pass Check
  -- Discard words of length below Min
  procedure List (Letters : in String;
                  Check : in Check_Access := null;
                  Min : in Positive := 1) is
    The_Check : Check_Access;
    Word : String (1 .. Letters'Length);
  begin
    -- Clear result and store Check access
    for I in Result'Range loop
      Result(I) := Asu_Ua.Null_Unb_Array;
    end loop;
    if Check = null then
      The_Check := Check_Ok'Access;
    else
      The_Check := Check;
    end if;

    -- Start analyse
    Permute (Letters, The_Check, Min, Letters'Length, 0, Word);

  end List;

end Analist;

