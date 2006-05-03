with Dynamic_List;
package body Words is

  package Words_Dyn_List is
     new Dynamic_List (Ada.Strings.Unbounded.Unbounded_String);
  package Words_List_Mng renames Words_Dyn_List.Dyn_List;
  Words_List : Words_List_Mng.List_Type;

  -- Reset --
  procedure Reset is
  begin
    Words_List_Mng.Delete_List (Words_List, False);
  end Reset;

  -- Get --
  function Get (Index : in Natural := 0) return String is
    Current, Result : Ada.Strings.Unbounded.Unbounded_String;
    Done : Boolean;
  begin
    if Index /= 0 then
      -- Move at Index
      if Index > Words_List_Mng.List_Length (Words_List) then
        return "";
      end if;
      Words_List_Mng.Move_To (
             List => Words_List,
             Where => Words_List_Mng.Next,
             Number => Index - 1,
             From_Current => False);
      -- Read word
      Words_List_Mng.Read (Words_List, Result, Words_List_Mng.Current);
    elsif not Words_List_Mng.Is_Empty (Words_List) then
      -- Concatenate all words
      Words_List_Mng.Rewind (Words_List);
      loop
        Words_List_Mng.Read (Words_List, Current, Done => Done);
        Ada.Strings.Unbounded.Append (Result, Current);
        exit when not Done;
      end loop;
    end if;
    return  Ada.Strings.Unbounded.To_String (Result);
  end Get;

  -- Returns the number of words stored
  function Length return Natural is
  begin
    return Words_List_Mng.List_Length (Words_List);
  end Length;

  -- Add --
  procedure Add (Word : in String) is
  begin
    Add (Ada.Strings.Unbounded.To_Unbounded_String (Word));
  end Add;

  procedure Add (Word : in Ada.Strings.Unbounded.Unbounded_String) is
  begin
    -- Append word
    if not Words_List_Mng.Is_Empty (Words_List) then
       Words_List_Mng.Rewind (Words_List, Words_List_Mng.Prev);
    end if;
    Words_List_Mng.Insert (Words_List, Word);
  end Add;

  -- Locate a word, returns 0 if not found
  function Match (Current, Criteria : Ada.Strings.Unbounded.Unbounded_String)
            return Boolean is
    use type Ada.Strings.Unbounded.Unbounded_String;
  begin
    return Current = Criteria;
  end Match;

  function Search (Word : String) return Natural is
    Found : Boolean;
  begin
    Words_List_Mng.Search_Match (Words_List, Found, Match'Access,
        Ada.Strings.Unbounded.To_Unbounded_String (Word),
        From => Words_List_Mng.Absolute);
    if not Found then
       return 0;
    else
      return Words_List_Mng.Get_Position (Words_List);
    end if;
  end Search;

end Words;

