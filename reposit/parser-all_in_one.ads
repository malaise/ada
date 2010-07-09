package Parser.All_In_One is
  use As.U;

  -- The array of parsed element
  -- A parsed element is either a word or some separators
  type Parsed_Element_Kind_List is (Word, Separators);
  type Parsed_Element_Rec is record
    Kind : Parsed_Element_Kind_List;
    Str : Asu_Us;
  end record;

  type Parsed_Array is array (Positive range <>) of Parsed_Element_Rec;


  -- Reset the iterator, parse all words and separators, reset the iterator
  --  and return the parsed array.
  -- May raise Constraint_Error if Iter is not set.
  function Parse_All (Iter : Iterator) return Parsed_Array;

end Parser.All_In_One;

