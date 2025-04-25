-- Parse the words of a string according to a separator
private with As.U;
package Parser is

  -- The function to define separating characters
  -- Exceptions raised by this function are propagated
  type Separing_Function is
       access function (C : Character) return Boolean;

  -- A convenient default separing function
  function Is_Space_Or_Htab_Function (C : Character) return Boolean;
  Space_Htab : constant Separing_Function
             := Is_Space_Or_Htab_Function'Access;

  -- The iterator
  type Iterator is tagged private;

  -- Initialise the iterator Iter with the string to parse and the criteria.
  -- Previous content of Iter is overwritten.
  -- May raise Constraint_Error if Is_Sep is null
  procedure Set (Iter : in out Iterator;
                 Str : in String;
                 Is_Sep : in not null Separing_Function := Space_Htab);

  -- Is the iterator Iter set?
  function Is_Set (Iter : Iterator) return Boolean;

  -- Delete the content of of the iterator Iter (which becomes not set).
  -- May raise Constraint_Error if Iter is not set.
  procedure Del (Iter : in out Iterator);

  -- Reset iterator Iter, setting it in the same state as at creation.
  -- The separing function may be changed at this occasion.
  -- May raise Constraint_Error if Iter is not set.
  procedure Reset (Iter : in out Iterator;
                   Is_Sep : in Separing_Function := null);

  -- Reset iterator Iter with a new Str and the same separing function
  -- May raise Constraint_Error if Iter is not set.
  procedure Reset (Iter : in out Iterator; Str : in String);

  -- Parse first then next word of the string.
  -- End of parsing is signaled by returning an empty string.
  -- May raise Constraint_Error if Iter is not set.
  function Next_Word (Iter : in out Iterator) return String;
  procedure Next_Word (Iter : in out Iterator);

  -- Return the separating characters previously found when looking
  --   for Next_Word.
  -- Just after Creation, returns an empty string.
  -- After Next_Word has returned a string:
  --  - an empty string if the word was the first and at string_start,
  --  - the separators from previous_word or string_start.
  -- After Next_Word has returned empty string:
  --  - an empty string if the string is empty,
  --  - the separators from previous_word or string:
  -- May raise Constraint_Error if Iter is not set.
  function Prev_Separators (Iter : Iterator) return String;

  -- Return current word.
  --  or an empty string if parsing is not started or finished.
  -- May raise Constraint_Error if Iter is not set.
  function Current_Word (Iter : Iterator) return String;

  -- Reset Iter and parse first word.
  -- May raise Constraint_Error if Iter is not set.
  function First_Word (Iter : in out Iterator) return String;
  procedure First_Word (Iter : in out Iterator);

  -- Return the indexes of current word.
  -- The Str provided to Set may not have been from 1 to N.
  -- If not Normalized, the indexes are in this Str,
  --  otherwise they are from 1 to N.
  -- Returns 1 .. 0 if parsing is not started or finished.
  -- May raise Constraint_Error if Iter is not set.
  function First_Index (Iter : Iterator; Normalize : Boolean := True)
                       return Positive;
  function Last_Index  (Iter : Iterator; Normalize : Boolean := True)
                       return Natural;

  -- Return the string to which the iterator was set.
  -- If not Normalized, the string returned has the same indexes as the initial,
  --  otherwise it is from 1 to N
  -- May raise Constraint_Error if Iter is not set.
  function Image (Iter : Iterator; Normalize : Boolean := True)
                 return String;

  -- Return the tail of string, including all separators after current word
  -- If parsing is not started then returns Image
  -- If parsing is finished then returns "",
  -- Otherwise, if not Normalized, the string returned has the same indexes
  -- as in the initial, otherwise it is from 1 to N
  -- May raise Constraint_Error if Iter is not set.
  function Tail (Iter : Iterator; Normalize : Boolean := True)
                 return String;

private

  type Iter_State_List is (Unset, Parsing, Parsed, Finished);

  -- Start is original Str'First
  -- While not Finished:
  --   Word is Str (First .. Last)
  --   Separators is Str (Sep .. First - 1);
  -- If Word is not empty then Indexes are First and Last, else 1 and 0.
  type Iterator is tagged record
    -- Initialised by Set
    -- Current parsing state
    State : Iter_State_List := Unset;
    -- The string, its lenght and first index, and separation function
    Str : As.U.Asu_Us := As.U.Asu_Null;
    Len : Natural := 0;
    Start : Positive := 1;
    Is_Sep : Separing_Function := null;
    -- Indexes of current word
    First : Natural := 1;
    Last  : Natural := 0;
    -- Index of the first separator (of the chain of seperators) before First
    Sep   : Natural := 1;
  end record;

end Parser;

