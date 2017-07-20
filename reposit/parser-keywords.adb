package body Parser.Keywords is

  -- Keywords and data stored in Hased list
  procedure Set (To : out Cell_Type; Val : in Cell_Type) is
  begin
    To := Val;
  end Set;
  overriding function "=" (Current : Cell_Type;
                           Criteria : Cell_Type) return Boolean is
    use type As.U.Asu_Us;
  begin
    return Current.Keyword = Criteria.Keyword;
  end "=";
  function Image (Cell : Cell_Type) return String is (Cell.Keyword.Image);


  -- Add a keyword
  -- Any redefinition of a keyword overwrites the previous
  procedure Add (Keys : in out Iterator;
                 Keyword : in As.U.Asu_Us; Data : in Data_Type) is
  begin
    Keys.List.Insert ( (Keyword, Data) );
  end Add;

  procedure Add (Keys : in out Iterator;
                 Keyword : in String; Data : in Data_Type) is
  begin
    Add (Keys, As.U.Tus (Keyword), Data);
  end Add;

  -- Clears all the words
  procedure Reset (Keys : in out Iterator) is
  begin
    Keys.List.Delete_List;
  end Reset;

  -- Set the sentence to parse, the parsing will start from the Next_Word
  procedure Set (Keys : in out Iterator; Iter : in Parser.Iterator) is
  begin
    Keys.Iter :=  Iter;
    Keys.End_Reached := False;
  end Set;

  -- Result of parsing
  -- Next_Word is a keyword, not a keyword, or end iterator is reached
  -- type Result_Kind is (Found, Not_Found, End_Reached);

  -- type Result_Rec (Kind : Result_Kind := Found) is record
  --   case Kind is
  --     when Found =>
  --       -- Keyword found
  --       Keyword : As.U.Asu_Us;
  --       Data    : Data_Type;
  --     when Not_Found =>
  --       Word : As.U.Asu_Us;
  --     when End_Reached => null;
  --   end case;
  -- end record;

  -- Parse next word
  -- Raises End_Error if End_Reached has already been returned
  function Next_Word (Keys : in out Iterator) return Result_Rec is
    Found : Boolean;
    Crit : Cell_Type;
  begin
    if Keys.End_Reached then
      -- End already reached
      raise End_Error;
    end if;
    -- Read next word of iterator
    Crit.Keyword.Set (Keys.Iter.Next_Word);
    if Crit.Keyword.Is_Null then
      -- End of iterator
      Keys.End_Reached := True;
      return (Kind => End_Reached);
    end if;
    -- Search keyword
    Keys.List.Search (Crit, Found);
    if Found then
      Keys.List.Read (Crit);
      return (Kind => Parser.Keywords.Found,
              Keyword => Crit.Keyword, Data => Crit.Data);
    else
      return (Kind => Not_Found, Word => Crit.Keyword);
    end if;
  end Next_Word;

end Parser.Keywords;

