--  with function Is_Separator (C : Character) return Boolean;
with Ada.Unchecked_Deallocation;
package body Parser is

  Empty : constant String := "";

  -- Initialise an iterator with the string to parse and the criteria
  -- May raise Constraint_Error if the String is too
  --   long (see Text_Handler.Max_Len_Range)
  procedure Create (Str : in String;
                    Is_Sep : in Separing_Function;
                    Iter : out Iterator) is
  begin
    if Str'Length > Text_Handler.Max_Len_Range'Last then
      raise Constraint_Error;
    end if;
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
    Iter := new Iter_Rec'(Str'Length, Str, Is_Sep, Parsing,
                 Init_Rec.First, Init_Rec.Last, Init_Rec.Sep);
  end Create;

  -- Check iterator validity
  procedure Check (Iter : Iterator) is
  begin
    if Iter = null
      then raise Constraint_Error;
    end if;
  end Check;
  
  -- Destroy the iterator
  -- May raise Constraint_Error if iterator has not been created
  procedure Free is new Ada.Unchecked_Deallocation(Object => Iter_Rec,
                                                   Name   => Iterator);

  procedure Delete (Iter : in out Iterator) is
  begin
    Check (Iter);
    Free (Iter);
  end Delete;

  -- Parse first then next word of the string
  -- Parsing ends by returning empty string
  -- May raise Constraint_Error if iterator has not been created
  function Next_Word (Iter : Iterator) return String is
  begin
    Check (Iter);
    -- Check if parsing is finished
    if Iter.State = Parsed then
      Iter.Sep := Iter.First;
      return Empty;
    end if;

    -- Check for first call to Next_Word on empty string
    if Iter.First > Iter.Len then
      Iter.State := Parsed;
      return Empty;
    end if;

    -- Init search of next character (non sep)
    Iter.First := Iter.Last + 1;

    if Iter.Is_Sep (Iter.Str(Iter.First)) then
      -- Skip separators
      Iter.Sep := Iter.First;
      loop
        exit when not Iter.Is_Sep (Iter.Str(Iter.First));
        if Iter.First = Iter.Len then
          -- String is terminating with separators
          Iter.State := Parsed;
          Iter.First := Iter.First + 1;
          return Empty;
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
      exit when Iter.Is_Sep (Iter.Str(Iter.Last + 1));
      Iter.Last := Iter.Last + 1;
    end loop;

    return Iter.Str(Iter.First .. Iter.Last);
        
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
    return Iter.Str(Iter.Sep .. Iter.First - 1);
  end Prev_Separators;

end Parser;


