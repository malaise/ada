with Dynamic_List;
package body Words is

  package Words_Dyn_List is
     new Dynamic_List (Word_Rec);
  package Words_List_Mng renames Words_Dyn_List.Dyn_List;
  Words_List : Words_List_Mng.List_Type;

  -- Reset --
  procedure Reset is
  begin
    Words_List.Delete_List (False);
  end Reset;

  -- Del
  procedure Del (Index : in Natural := 0) is
  begin
    if Words_List.Is_Empty then
      return;
    end if;
    if Index = 0 or else Index =  Words_List.List_Length then
      -- Delete last word
      Words_List.Rewind (True, Words_List_Mng.Prev);
      Words_List.Delete (Words_List_Mng.Prev);
    elsif Index > Words_List.List_Length then
      return;
    else
      -- Delete word at index
      Words_List.Move_At (Index);
      Words_List.Delete;
    end if;

  end Del;

  -- Read --
  function Read (Index : in Natural := 0) return Word_Rec is
    Read_Index : Positive;
    Word : Word_Rec;
  begin
    if Words_List.Is_Empty
    or else Index > Length then
      return (Parser_Ada.Separator, Tus (""));
    end if;
    if Index /= 0 then
      Read_Index := Index;
    else
      Read_Index := Length;
    end if;
    -- Move at Index
    Words_List.Move_At (Read_Index);
    -- Read word
    Words_List.Read (Word, Words_List_Mng.Current);
    return Word;
  end Read;

  function Read (Index : in Natural := 0) return Asu_Us is
  begin
    return Read (Index).Text;
  end Read;

  function Read (Index : in Natural := 0) return String is
  begin
    return Read (Index).Image;
  end Read;

  -- Get --
  function Get (Index : in Natural := 0) return Word_Rec is
    Word : Word_Rec;
  begin
    Word := Read (Index);
    Del (Index);
    return Word;
  end Get;

  function Get (Index : in Natural := 0) return Asu_Us is
  begin
    return Get (Index).Text;
  end Get;

  function Get (Index : in Natural := 0) return String is
  begin
    return Get (Index).Image;
  end Get;

  -- Concat --
  function Concat (From_Index : in Positive := 1;
                   To_Index : Natural := 0) return Asu_Us is
    Result : Asu_Us;
    Last : Natural;
  begin
    if To_Index /= 0 then
      Last := To_Index;
    else
      Last := Length;
    end if;

    for I in From_Index .. Last loop
      Result.Append (Asu_Us'(Read (I)));
    end loop;
    return Result;
  end Concat;

  function Concat (From_Index : in Positive := 1;
                   To_Index : Natural := 0) return String is
  begin
    return Concat (From_Index, To_Index).Image;
  end Concat;


  -- Returns the number of words stored
  function Length return Natural is
  begin
    return Words_List.List_Length;
  end Length;

  -- Add --
  procedure Add (Word : in Word_Rec) is
  begin
    -- Append word
    Words_List.Rewind (False, Words_List_Mng.Prev);
    Words_List.Insert (Word);
  end Add;

  procedure Add (Lexic : in Parser_Ada.Lexical_Kind_List; Text : in Asu_Us) is
  begin
    Add ( (Lexic, Text) );
  end Add;

  procedure Add (Lexic : in Parser_Ada.Lexical_Kind_List;
                 Text : in String) is
  begin
    Add (Lexic, Tus (Text));
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
    Words_List.Move_At (From_Index);
    Words_List.Search_Match (Found, Match'Access, Word,
                             From => Words_List_Mng.From_Current);
    if not Found then
       return 0;
    else
      return Words_List.Get_Position;
    end if;
  end Search;

  function Search (Lexic : in Parser_Ada.Lexical_Kind_List;
                   Word : in Asu_Us;
                   From_Index : Positive := 1) return Natural is

  begin
    return Search ( (Lexic, Word), From_Index );
  end Search;

  function Search (Lexic : Parser_Ada.Lexical_Kind_List;
                   Word : String;
                   From_Index : Positive := 1) return Natural is
  begin
    return Search (Lexic, Tus (Word), From_Index);
  end Search;

end Words;

