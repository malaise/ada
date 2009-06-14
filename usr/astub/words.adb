with Dynamic_List;
package body Words is

  package Words_Dyn_List is
     new Dynamic_List (Word_Rec);
  package Words_List_Mng renames Words_Dyn_List.Dyn_List;
  Words_List : Words_List_Mng.List_Type;

  -- Reset --
  procedure Reset is
  begin
    Words_List_Mng.Delete_List (Words_List, False);
  end Reset;

  -- Del
  procedure Del (Index : in Natural := 0) is
  begin
    if Words_List_Mng.Is_Empty (Words_List) then
      return;
    end if;
    if Index = 0 or else Index =  Words_List_Mng.List_Length (Words_List) then
      -- Delete last word
      Words_List_Mng.Rewind (Words_List, Words_List_Mng.Prev);
      Words_List_Mng.Delete (Words_List, Words_List_Mng.Prev);
    elsif Index > Words_List_Mng.List_Length (Words_List) then
      return;
    else
      -- Delete word at index
      Words_List_Mng.Move_To (Words_List, Words_List_Mng.Next,
                              Index - 1, False);
      Words_List_Mng.Delete (Words_List);
    end if;

  end Del;

  -- Read --
  function Read (Index : in Natural := 0) return Word_Rec is
    Read_Index : Positive;
    Word : Word_Rec;
  begin
    if Words_List_Mng.Is_Empty (Words_List)
    or else Index > Length then
      return (Parser_Ada.Separator,
              Ada.Strings.Unbounded.To_Unbounded_String (""));
    end if;
    if Index /= 0 then
      Read_Index := Index;
    else
      Read_Index := Length;
    end if;
    -- Move at Index
    Words_List_Mng.Move_To (
           List => Words_List,
           Where => Words_List_Mng.Next,
           Number => Read_Index - 1,
           From_Current => False);
    -- Read word
    Words_List_Mng.Read (Words_List, Word, Words_List_Mng.Current);
    return Word;
  end Read;

  function Read (Index : in Natural := 0)
                return Ada.Strings.Unbounded.Unbounded_String is
  begin
    return Read (Index).Text;
  end Read;

  function Read (Index : in Natural := 0) return String is
  begin
    return Ada.Strings.Unbounded.To_String (Read (Index));
  end Read;

  -- Get --
  function Get (Index : in Natural := 0) return Word_Rec is
    Word : Word_Rec;
  begin
    Word := Read (Index);
    Del (Index);
    return Word;
  end Get;

  function Get (Index : in Natural := 0)
                return Ada.Strings.Unbounded.Unbounded_String is
  begin
    return Get (Index).Text;
  end Get;

  function Get (Index : in Natural := 0) return String is
  begin
    return Ada.Strings.Unbounded.To_String (Get (Index));
  end Get;

  -- Concat --
  function Concat (From_Index : in Positive := 1;
                   To_Index : Natural := 0)
           return Ada.Strings.Unbounded.Unbounded_String is
    Result : Ada.Strings.Unbounded.Unbounded_String;
    Last : Natural;
  begin
    if To_Index /= 0 then
      Last := To_Index;
    else
      Last := Length;
    end if;

    for I in From_Index .. Last loop
      Ada.Strings.Unbounded.Append (Result, String'(Read (I)));
    end loop;
    return Result;
  end Concat;

  function Concat (From_Index : in Positive := 1;
                   To_Index : Natural := 0) return String is
  begin
    return Ada.Strings.Unbounded.To_String (Concat (From_Index, To_Index));
  end Concat;


  -- Returns the number of words stored
  function Length return Natural is
  begin
    return Words_List_Mng.List_Length (Words_List);
  end Length;

  -- Add --
  procedure Add (Word : in Word_Rec) is
  begin
    -- Append word
    if not Words_List_Mng.Is_Empty (Words_List) then
       Words_List_Mng.Rewind (Words_List, Words_List_Mng.Prev);
    end if;
    Words_List_Mng.Insert (Words_List, Word);
  end Add;

  procedure Add (Lexic : in Parser_Ada.Lexical_Kind_List;
                 Text : in Ada.Strings.Unbounded.Unbounded_String) is
  begin
    Add ( (Lexic, Text) );
  end Add;

  procedure Add (Lexic : in Parser_Ada.Lexical_Kind_List;
                 Text : in String) is
  begin
    Add (Lexic, Ada.Strings.Unbounded.To_Unbounded_String (Text));
  end Add;

  -- Search --
  -- Locate a word, returns 0 if not found
  function Match (Current, Criteria : Word_Rec) return Boolean is
    use type Parser_Ada.Word_Rec;
  begin
    return Current = Criteria;
  end Match;

  function Search (Word : Word_Rec;
                   From_Index : Positive := 1) return Natural is
    Found : Boolean;
  begin
    Words_List_Mng.Move_To (Words_List, Words_List_Mng.Next,
                  From_Index - 1, From_Current => False);
    Words_List_Mng.Search_Match (Words_List, Found, Match'Access,
        Word, From => Words_List_Mng.From_Current);
    if not Found then
       return 0;
    else
      return Words_List_Mng.Get_Position (Words_List);
    end if;
  end Search;

  function Search (Lexic : in Parser_Ada.Lexical_Kind_List;
                   Word : in Ada.Strings.Unbounded.Unbounded_String;
                   From_Index : Positive := 1) return Natural is

  begin
    return Search ( (Lexic, Word), From_Index );
  end Search;

  function Search (Lexic : Parser_Ada.Lexical_Kind_List;
                   Word : String;
                   From_Index : Positive := 1) return Natural is
  begin
    return Search (Lexic,
                   Ada.Strings.Unbounded.To_Unbounded_String (Word),
                   From_Index);
  end Search;

end Words;

