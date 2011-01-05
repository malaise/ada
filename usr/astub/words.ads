-- Stores the words and allows retrieval
with As.U;
with Parser_Ada;
package Words is

  -- A Word as it is stored
  subtype Word_Rec is Parser_Ada.Word_Rec;

  -- Not used here, but usefull
  type Word_Array is array (Positive range <>) of Word_Rec;

  -- Empty word array
  No_Word : constant Word_Array (1 .. 0)
          := (others => (Lexic => Parser_Ada.Comment,
                         Text => As.U.Tus ("")));

  -- Reset the stored words
  procedure Reset;

  -- Remove a word (nothing of list is empty or index bigger than length)
  -- If Index is 0 then remove last
  procedure Del (Index : in Natural := 0);

  -- Retrieves one word
  -- If Index is 0 then read last
  -- Returns "" (and Separator) if no such word
  function Read (Index : in Natural := 0) return Word_Rec;
  function Read (Index : in Natural := 0) return As.U.Asu_Us;
  function Read (Index : in Natural := 0) return String;

  -- Retrieves and removes one word
  -- If Index is 0 then read last
  -- Returns "" (and Separator) if no such word
  function Get (Index : in Natural := 0) return Word_Rec;
  function Get (Index : in Natural := 0) return As.U.Asu_Us;
  function Get (Index : in Natural := 0) return String;

  -- Retrieve and concatenate several words,
  --  from From_Index to To_Index (0 for last) included
  function Concat (From_Index : in Positive := 1;
                   To_Index : Natural := 0) return As.U.Asu_Us;
  function Concat (From_Index : in Positive := 1;
                   To_Index : Natural := 0) return String;

  -- Returns the number of words stored
  function Length return Natural;

  -- Store (appends) one word
  procedure Add (Word : in Word_Rec);
  procedure Add (Lexic : in Parser_Ada.Lexical_Kind_List;
                 Text : in As.U.Asu_Us);
  procedure Add (Lexic : in Parser_Ada.Lexical_Kind_List;
                 Text : in String);


  -- Locate a word, starting from From_Index excluded,
  --  returns 0 if not found
  function Search (Word : Word_Rec;
                   From_Index : Positive := 1) return Natural;
  function Search (Lexic : in Parser_Ada.Lexical_Kind_List;
                   Word : in As.U.Asu_Us;
                   From_Index : Positive := 1) return Natural;
  function Search (Lexic : Parser_Ada.Lexical_Kind_List;
                   Word : String;
                   From_Index : Positive := 1) return Natural;

end Words;

