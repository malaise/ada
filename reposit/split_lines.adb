function Split_Lines (Iter : Parser.Iterator;
                      Len  : Positive;
                      Max : Natural := 0) return As.U.Utils.Asu_Ua.Unb_Array is
  Result : As.U.Utils.Asu_Ua.Unb_Array;
  Line : As.U.Asu_Us;
  Needed : Positive;
  Num : Positive;
begin
  -- Iterate on each word
  Num := 1;
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
      if Line.Length + Needed <= Len or else Num = Max then
        -- Append Word to Line, with a space except if first word of line
        if not Line.Is_Null then
          Line.Append (" ");
        end if;
        Line.Append (Word);
      else
        -- New line, store previous
        if not Line.Is_Null then
          Result.Append (Line);
          Num := Num + 1;
        end if;
        Line.Set (Word);
      end if;
    end;
  end loop;

  -- Append last line if any
  if not Line.Is_Null then
     Result.Append (Line);
  end if;

  return Result;
end Split_Lines;

