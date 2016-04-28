with Str_Util;
package body Parser is

  -- Standard seaparing function
  function Is_Space_Or_Htab_Function (C : Character) return Boolean renames
           Str_Util.Is_Separator;

  -- Is iterator created and not deleted
  function Is_Set (Iter : Iterator) return Boolean is
  begin
    return Iter.State /= Unset;
  end Is_Set;

  procedure Clear (Iter : in out Iterator) is
  begin
    Iter := (others => <>);
  end Clear;

  -- Initialise an iterator with the string to parse and the criteria
  -- May raise Constraint_Error if the String is too
  procedure Set (Iter : in out Iterator;
                 Str  : in String;
                 Is_Sep : in Separing_Function := Space_Htab) is
  begin
    if Is_Sep = null then
      raise Constraint_Error;
    end if;
    declare
      Dummy : Boolean;
    begin
      Dummy := Is_Sep ('-');
    exception
      when others =>
        raise Constraint_Error;
    end;
    Clear (Iter);
    Iter := (
      State => Parsing,
      Str => As.U.Tus (Str),
      Len => Str'Length,
      Start => Str'First,
      Is_Sep => Is_Sep,
      others => <>);
  end Set;


  -- Check iterator validity
  procedure Check (Iter : Iterator) is
  begin
    if not Is_Set (Iter) then
      raise Constraint_Error;
    end if;
  end Check;

  -- Destroy the iterator
  -- May raise Constraint_Error if iterator has not been created
  procedure Del (Iter : in out Iterator) is
  begin
    Check (Iter);
    Clear (Iter);
  end Del;

  -- Reset the iterator, setting it in the same state than at creation
  -- The separing function may be changed
  -- May raise Constraint_Error if iterator has not been created or is deleted
  procedure Reset (Iter : in out Iterator;
                   Is_Sep : in Separing_Function := null) is
  begin
    Check (Iter);
    -- Same string and possibly new separing function
    Iter := (
      State  => Parsing,
      Str    => Iter.Str,
      Len    => Iter.Len,
      Start  => Iter.Start,
      Is_Sep => (if Is_Sep /= null then Is_Sep else Iter.Is_Sep),
      others => <>);
  end Reset;

  -- Reset iterator Iter with a new Str and the same separing function
  -- May raise Constraint_Error if Iter is not set.
  procedure Reset (Iter : in out Iterator; Str : in String) is
  begin
    Check (Iter);
    -- New string and same separing function
    Iter := (
      State  => Parsing,
      Str    => As.U.Tus (Str),
      Len    => Str'Length,
      Start  => Str'First,
      Is_Sep => Iter.Is_Sep,
      others => <>);
  end Reset;

  -- Parse first then next word of the string
  -- Parsing ends by returning empty string
  -- May raise Constraint_Error if iterator has not been created
  procedure Next_Word (Iter : in out Iterator) is
  begin
    Check (Iter);

    -- Check if parsing is finished
    if Iter.State = Finished then
      return;
    end if;
    if Iter.State = Parsed then
      -- Reset state ans set to Finished
      Reset (Iter, Iter.Is_Sep);
      Iter.State := Finished;
      return;
    end if;

    -- Check for first call to Next_Word on empty string
    if Iter.First > Iter.Len then
      Reset (Iter, Iter.Is_Sep);
      Iter.State := Finished;
      return;
    end if;

    -- Init search of next character (non sep)
    Iter.First := Iter.Last + 1;

    if Iter.Is_Sep (Iter.Str.Element (Iter.First)) then
      -- Skip separators
      Iter.Sep := Iter.First;
      while Iter.Is_Sep (Iter.Str.Element (Iter.First)) loop
        if Iter.First = Iter.Len then
          -- String is terminating with separators
          Iter.State := Parsed;
          Iter.First := Iter.First + 1;
          return;
        end if;
        Iter.First := Iter.First + 1;
      end loop;
    end if;

    -- Now Iter.First is first character
    Iter.Last := Iter.First;
    -- Skip characters
    loop
      if Iter.Last = Iter.Len then
        -- String is terminating with word
        Iter.State := Parsed;
        exit;
      end if;
      exit when Iter.Is_Sep (Iter.Str.Element (Iter.Last + 1));
      Iter.Last := Iter.Last + 1;
    end loop;

  end Next_Word;

  function Next_Word (Iter : in out Iterator) return String is
  begin
    Next_Word (Iter);
    return Current_Word (Iter);
  end Next_Word;


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
  function Prev_Separators (Iter : Iterator) return String is
  begin
    Check (Iter);
    return Iter.Str.Slice (Iter.Sep,  Iter.First - 1);
  end Prev_Separators;


  -- Return current word
  -- Empty string if parsing not started or finished
  -- May raise Constraint_Error if iterator has not been created
  function Current_Word (Iter : Iterator) return String is
  begin
    Check (Iter);
    return Iter.Str.Slice (Iter.First, Iter.Last);
  end Current_Word;

  -- Reset and parse first word
  function First_Word (Iter : in out Iterator) return String is
  begin
    Reset (Iter);
    return Next_Word (Iter);
  end First_Word;

  procedure First_Word (Iter : in out Iterator) is
  begin
    Reset (Iter);
    Next_Word (Iter);
  end First_Word;


  -- Return the indexes of current word
  -- 1 .. 0 if parsing not started or finished
  -- May raise Constraint_Error if iterator has not been created
  function First_Index (Iter : Iterator; Normalize : Boolean := True)
                       return Positive is
  begin
    Check (Iter);
    return (if Iter.First > Iter.Last then 1
            elsif Normalize then Iter.First
            else Iter.First - 1 + Iter.Start);
  end First_Index;

  function Last_Index  (Iter : Iterator; Normalize : Boolean := True)
                       return Natural is
  begin
    Check (Iter);
    return (if Iter.First > Iter.Last then 0
            elsif Normalize then Iter.Last
            else Iter.Last - 1 + Iter.Start);
  end Last_Index;

  -- Return the string Str with which the iterator was created
  -- The Str provided to Create may not have been from 1 to N
  -- If not Normalized, the string returned has the same range as Str
  --  otherwise they are from 1 to N
  -- May raise Constraint_Error if iterator does not exist
  function Image (Iter : Iterator; Normalize : Boolean := True)
                 return  String is
  begin
    Check (Iter);
    if Normalize then
      return Iter.Str.Slice (1, Iter.Len);
    else
      -- Return a string from Start to N
      declare
        Str : constant String (Iter.Start .. Iter.Start+Iter.Len-1)
            := Iter.Str.Slice (1, Iter.Len);
      begin
        return Str;
      end;
    end if;
  end Image;

  -- Return the tail of string, including all separators after current word
  -- If parsing is not started then returns Image
  -- If parsing is finished then returns "",
  -- Otherwise, if not Normalized, the string returned has the same indexes
  -- as in the initial, otherwise it is from 1 to N
  -- May raise Constraint_Error if Iter is not set.
  function Tail (Iter : Iterator; Normalize : Boolean := True)
                 return String is
  begin
    Check (Iter);
    if Iter.State = Finished then
      return "";
    end if;
    if Iter.Current_Word = "" then
      -- Parsing not started
      return Image (Iter, Normalize);
    elsif Normalize then
      return Iter.Str.Slice (Iter.Last+1, Iter.Len);
    else
      -- Return a string from Start to N
      declare
        Str : constant String (Iter.Start+Iter.Last
                            .. Iter.Start+Iter.Len-1)
            := Iter.Str.Slice (Iter.Last+1, Iter.Len);
      begin
        return Str;
      end;
    end if;
  end Tail;

end Parser;

