function Split_Lines (Iter : Parser.Iterator;
                      Len  : Positive;
                      Max  : Integer := 0;
                      Cut  : Boolean := True)
                      return As.U.Utils.Asu_Ua.Unb_Array is
  Result : As.U.Utils.Asu_Ua.Unb_Array;
  Line : As.U.Asu_Us;
  Needed : Positive;
  use type As.U.Asu_Us;
begin
  -- Split input into lines of fixed max len
  -- Iterate on each word
  loop
    declare
      Word : constant String := Iter.Next_Word;
    begin
      exit when Word = "";

      -- Needed len: also append a space if not first word
      Needed := Word'Length;
      if not Line.Is_Null then
        Needed := Needed + 1;
      end if;

      -- Check if we can append
      if Line.Length + Needed <= Len then
        -- Append Word to Line, with a space except if first word of line
        if not Line.Is_Null then
          Line.Append (" ");
        end if;
        Line.Append (Word);
      else
        -- New line, store previous
        if not Line.Is_Null then
          Result.Append (Line);
        end if;
        Line.Set (Word);
      end if;
    end;
  end loop;

  -- Append last line if any
  if not Line.Is_Null then
     Result.Append (Line);
  end if;

  if Max = 0 or else Result.Length <= abs Max then
    -- Merge/Cut head or tail only if necessary
    return Result;
  end if;

  if not Cut then
    -- Merge
    if Max > 0 then
      -- Merge tailing lines
      Line := Result.Element (Max);
      for I in Max + 1 .. Result.Length loop
        Line.Append (" " & Result.Element (I));
      end loop;
      Result.Replace_Element (Max, Line);
    else
      -- Merge heading lines (keep in mind that Max < 0)
      Line := Result.Element (Result.Length + Max + 1);
      for I in reverse 1 .. Result.Length + Max loop
        Line.Prepend (Result.Element (I) & " ");
      end loop;
      Result.Replace_Element (Result.Length + Max + 1, Line);
    end if;
  end if;

  -- Cut anyway
  if Max > 0 then
    -- Cut tailing lines
    Result.Delete (Max + 1, Result.Length);
  else
    -- Cut heading lines (keep in mind that Max < 0)
    Result.Delete (1, Result.Length + Max);
  end if;

  -- Done
  return Result;
end Split_Lines;

