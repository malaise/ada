-- Parser of keywords
private with Hashed_List.Unique;
with As.U;
generic
  -- Private data stored with the keyword
  type Data_Type is private;
package Parser.Keywords is

  -- Keywords parser iterator
  -- It allows storing several keywords (each with some private data),
  --  then setting a sentence (Parser.Iterator), then getting the words
  --  of the sentence one after the other; if a word is a keyword then
  --  it is retrieved togther with the private data.
  type Iterator is tagged limited private;

  -- Add a keyword
  -- Any redefinition of a keyword overwrites the previous
  procedure Add (Keys : in out Iterator;
                 Keyword : in String; Data : in Data_Type);
  procedure Add (Keys : in out Iterator;
                 Keyword : in As.U.Asu_Us; Data : in Data_Type);
  -- Clears all the words
  procedure Reset (Keys : in out Iterator);

  -- Set the sentence to parse, the parsing will start from the Next_Word
  procedure Set (Keys : in out Iterator; Iter : in Parser.Iterator);

  -- Result of parsing
  -- Next_Word is a keyword, not a keyword, or end iterator is reached
  type Result_Kind is (Found, Not_Found, End_Reached);
  type Result_Rec (Kind : Result_Kind := Found) is record
    case Kind is
      when Found =>
        -- Keyword found
        Keyword : As.U.Asu_Us;
        Data    : Data_Type;
      when Not_Found =>
        Word : As.U.Asu_Us;
      when End_Reached => null;
    end case;
  end record;

  -- Parse next word
  -- Raises End_Error if End_Reached has already been returned
  End_Error : exception;
  function Next_Word (Keys : in out Iterator) return Result_Rec;

private

  type Cell_Type is record
    Keyword : As.U.Asu_Us;
    Data : Data_Type;
  end record;
  procedure Set (To : out Cell_Type; Val : in Cell_Type);
  overriding function "=" (Current : Cell_Type;
                           Criteria : Cell_Type) return Boolean;
  function Image (Cell : Cell_Type) return String;
  package H_Cell_List_Mng is new Hashed_List (Cell_Type, Set, "=", Image);
  package Cell_List_Mng is new H_Cell_List_Mng.Unique;

  type Iterator is tagged limited record
    List : Cell_List_Mng.Unique_List_Type;
    End_Reached : Boolean := False;
    Iter : Parser.Iterator;
  end record;

end Parser.Keywords;

