with Ada.Characters.Latin_1, Ada.Unchecked_Deallocation;
package body Parser is

  -- Standard seaparing function
  function Is_Space_Or_Htab_Function (C : Character) return Boolean is
  begin
    return C = ' ' or else C = Ada.Characters.Latin_1.Ht;
  end Is_Space_Or_Htab_Function;

  -- Is iterator created and not deleted
  function Is_Set (Iter : Iterator) return Boolean is
  begin
    return Iter.Acc /= null;
  end Is_Set;

  -- Clear iterator if needed
  procedure Free is new Ada.Unchecked_Deallocation(Object => Iter_Rec,
                                                   Name   => Iter_Rec_Access);
  procedure Clear (Iter : in out Iterator) is
  begin
    if Is_Set (Iter) then
      Free (Iter.Acc);
    end if;
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
      pragma Unreferenced (Dummy);
    begin
      Dummy := Is_Sep ('-');
    exception
      when others =>
        raise Constraint_Error;
    end;
    Clear (Iter);
    Iter.Acc := new Iter_Rec'(As.U.Tus (Str), Str'Length,
                    Str'First, Is_Sep, Parsing,
                    Init_Rec.First, Init_Rec.Last, Init_Rec.Sep);
  end Set;


  -- Copy an iterator
  procedure Copy (Dest_Iter : in out Iterator; Src_Iter : in Iterator) is
  begin
    Clear (Dest_Iter);
    if Is_Set (Src_Iter) then
      Dest_Iter.Acc := new Iter_Rec'(Src_Iter.Acc.all);
    end if;
  end Copy;

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

  -- Automatic garbage collection
  overriding procedure Finalize (Iter : in out Iterator) is
  begin
    if Is_Set (Iter) then
      Del (Iter);
    end if;
  end Finalize;

  -- Reset the iterator, setting it in the same state than at creation
  -- The separing function may be changed
  -- May raise Constraint_Error if iterator has not been created or is deleted
  procedure Reset (Iter : in Iterator;
                   Is_Sep : in Separing_Function := null) is
  begin
    Check (Iter);
    -- Possibly new separator function
    if Is_Sep /= null then
      Iter.Acc.Is_Sep := Is_Sep;
    end if;
    Iter.Acc.State := Parsing;
    Iter.Acc.First := 1;
    Iter.Acc.Last  := 0;
    Iter.Acc.Sep   := 1;
  end Reset;

  -- Parse first then next word of the string
  -- Parsing ends by returning empty string
  -- May raise Constraint_Error if iterator has not been created
  procedure Next_Word (Iter : in Iterator) is
  begin
    Check (Iter);

    -- Check if parsing is finished
    if Iter.Acc.State = Finished then
      return;
    end if;
    if Iter.Acc.State = Parsed then
      Iter.Acc.First := 1;
      Iter.Acc.Last  := 0;
      Iter.Acc.Sep   := 1;
      Iter.Acc.State := Finished;
      return;
    end if;

    -- Check for first call to Next_Word on empty string
    if Iter.Acc.First > Iter.Acc.Len then
      Iter.Acc.State := Finished;
      return;
    end if;

    -- Init search of next character (non sep)
    Iter.Acc.First := Iter.Acc.Last + 1;

    if Iter.Acc.Is_Sep (Iter.Acc.Str.Element (Iter.Acc.First)) then
      -- Skip separators
      Iter.Acc.Sep := Iter.Acc.First;
      loop
        exit when not Iter.Acc.Is_Sep (Iter.Acc.Str.Element (Iter.Acc.First));
        if Iter.Acc.First = Iter.Acc.Len then
          -- String is terminating with separators
          Iter.Acc.State := Parsed;
          Iter.Acc.First := Iter.Acc.First + 1;
          return;
        end if;
        Iter.Acc.First := Iter.Acc.First + 1;
      end loop;
    end if;

    -- Now Iter.First is first character
    Iter.Acc.Last := Iter.Acc.First;
    -- Skip characters
    loop
      if Iter.Acc.Last = Iter.Acc.Len then
        -- String is terminating with word
        Iter.Acc.State := Parsed;
        exit;
      end if;
      exit when Iter.Acc.Is_Sep (Iter.Acc.Str.Element (Iter.Acc.Last + 1));
      Iter.Acc.Last := Iter.Acc.Last + 1;
    end loop;

  end Next_Word;

  function Next_Word (Iter : Iterator) return String is
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
    return Iter.Acc.Str.Slice (Iter.Acc.Sep,  Iter.Acc.First - 1);
  end Prev_Separators;


  -- Return current word
  -- Empty string if parsing not started or finished
  -- May raise Constraint_Error if iterator has not been created
  function Current_Word (Iter : Iterator) return String is
  begin
    Check (Iter);
    return Iter.Acc.Str.Slice (Iter.Acc.First, Iter.Acc.Last);
  end Current_Word;

  -- Reset and parse first word
  function First_Word (Iter : Iterator) return String is
  begin
    Reset (Iter);
    return Next_Word (Iter);
  end First_Word;

  procedure First_Word (Iter : in Iterator) is
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
    return (if Iter.Acc.First > Iter.Acc.Last then 1
            elsif Normalize then Iter.Acc.First
            else Iter.Acc.First - 1 + Iter.Acc.Start);
  end First_Index;

  function Last_Index  (Iter : Iterator; Normalize : Boolean := True)
                       return Natural is
  begin
    Check (Iter);
    return (if Iter.Acc.First > Iter.Acc.Last then 0
            elsif Normalize then Iter.Acc.Last
            else Iter.Acc.Last - 1 + Iter.Acc.Start);
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
      return Iter.Acc.Str.Slice (1, Iter.Acc.Len);
    else
      -- Return a string from Start to N
      declare
        Str : constant String (Iter.Acc.Start .. Iter.Acc.Start+Iter.Acc.Len-1)
            := Iter.Acc.Str.Slice (1, Iter.Acc.Len);
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
    if Iter.Acc.State = Finished then
      return "";
    end if;
    if Iter.Current_Word = "" then
      -- Parsing not started
      return Image (Iter, Normalize);
    elsif Normalize then
      return Iter.Acc.Str.Slice (Iter.Acc.Last+1, Iter.Acc.Len);
    else
      -- Return a string from Start to N
      declare
        Str : constant String (Iter.Acc.Start+Iter.Acc.Last
                            .. Iter.Acc.Start+Iter.Acc.Len-1)
            := Iter.Acc.Str.Slice (Iter.Acc.Last+1, Iter.Acc.Len);
      begin
        return Str;
      end;
    end if;
  end Tail;

end Parser;

