with Text_Handler;
package Parser is

  type Separing_Function is
       access function (C : Character) return Boolean;

  type Iterator is limited private;

  -- Initialise an iterator with the string to parse and the criteria
  -- May raise Constraint_Error if Is_Sep is null or invalid
  --  or if the String is too long (see Text_Handler.Max_Len_Range)
  procedure Create (Str : in String;
                    Is_Sep : in Separing_Function;
                    Iter : out Iterator);

  -- Destroy the iterator
  -- May raise Constraint_Error if iterator has not been created or is deleted
  procedure Delete (Iter : in out Iterator);

  -- Reset the iterator, setting it in the same state than at creation
  -- The separing function may be changed
  -- May raise Constraint_Error if iterator has not been created or is deleted
  procedure Reset (Iter : in out Iterator;
                   Is_Sep : in Separing_Function := null);

  -- Parse first then next word of the string
  -- Parsing ends by returning empty string
  -- May raise Constraint_Error if iterator has not been created
  function Next_Word (Iter : Iterator) return String;

  -- Return the separating characters previously found when looking
  --   for Next_Word
  -- Just after Creation, returns empty string
  -- After Next_Word returns a string
  --  - empty string if the word was the first and at string_start
  --  - the separators from previous_word or string_start
  -- After Next_Word returns empty string
  --  - empty string if the string is empty
  --  - the separators from previous_word or string
  -- May raise Constraint_Error if iterator has not been created
  function Prev_Separators (Iter : Iterator) return String;

private


  type Iter_State_List is (Parsing, Parsed);

  -- Word is Str (First .. Last)
  -- Separators is Str (Sep .. First - 1);
  type Iter_Rec (Len : Text_Handler.Max_Len_Range := 0) is record
    Str : String (1 .. Len) := (others => ' ');
    Is_Sep : Separing_Function := null;
    State : Iter_State_List := Parsing;
    First : Natural := 1;
    Last  : Natural := 0;
    Sep   : Natural := 1;
  end record;

  Default_Rec : Iter_Rec;
  Init_Rec : constant Iter_Rec := Default_Rec;

  type Iterator is access Iter_Rec;

end Parser;


