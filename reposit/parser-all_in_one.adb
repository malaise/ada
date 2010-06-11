with Dynamic_List;
package body Parser.All_In_One is


  package Parsed_Dynamic_List_Manager is new Dynamic_List (Parsed_Element_Rec);
  package Parsed_List_Manager renames Parsed_Dynamic_List_Manager.Dyn_List;
  Parsed_List : Parsed_List_Manager.List_Type;

  -- The array of parsed element
  -- A parsed element is either a word or some separators
  -- type Parsed_Element_Kind_List is (Word, Separators);

  -- type Parsed_Element_Rec is record
  --   Kind : Parsed_Element_Kind_List;
  --   Str : Ada.Strings.Unbounded.Unbounded_String;
  -- end record;


  -- type Parsed_Array is array (Positive range <>) of Parsed_Element_Rec;



  -- Reset the iterator, parse all words and separators, reset the iterator
  --  and return the parsed array.
  -- May raise Constraint_Error if Iter is not set.
  function Parse_All (Iter : Iterator) return Parsed_Array is
    Word_Elt, Seps_Elt : Parsed_Element_Rec;
    use type Ada.Strings.Unbounded.Unbounded_String;
  begin
    -- Parse Iterator, store separators then word into list
    Reset (Iter);
    Word_Elt.Kind := Word;
    Seps_Elt.Kind := Separators;
    loop
      Word_Elt.Str := Ada.Strings.Unbounded.To_Unbounded_String (
                      Next_Word (Iter));
      Seps_Elt.Str := Ada.Strings.Unbounded.To_Unbounded_String (
                      Prev_Separators (Iter));
      -- Store previous seperators if any (may not be the case before first
      --  word or after last word)
      if Seps_Elt.Str /= Ada.Strings.Unbounded.Null_Unbounded_String then
        Parsed_List.Insert (Seps_Elt);
      end if;
      -- Store word if any, note that an empty string leads to both empty Elts
      --  then to an empty list
      if Word_Elt.Str /= Ada.Strings.Unbounded.Null_Unbounded_String then
        Parsed_List.Insert (Word_Elt);
      else
        -- No more word: exit loop
        exit;
      end if;
    end loop;
    Reset (Iter);

    -- Create the array and copy list into it
    declare
      Result : Parsed_Array (1 .. Parsed_List.List_Length);
    begin
      if Result'Length /= 0 then
        Parsed_List.Rewind;
        -- Copy and delete list
        for I in 1 .. Result'Length loop
          Parsed_List.Get (Result(I));
        end loop;
      end if;
      return Result;
    end;
  end Parse_All;

end Parser.All_In_One;


