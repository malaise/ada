-- Stores the words and allows retrieval
with Ada.Strings.Unbounded;

package Words is

  -- Reset the stored words
  procedure Reset;

  -- Retrieves one word
  -- Retrieves the whole line if Index is 0
  -- Returns "" if no such word
  function Get (Index : in Natural := 0) return String;

  -- Returns the number of words stored
  function Length return Natural;

  -- Store (appends) one word
  procedure Add (Word : in String);
  procedure Add (Word : in Ada.Strings.Unbounded.Unbounded_String);

  -- Remove a word (nothing of list is empty or index bigger than length)
  -- If Index is 0 then remove last
  procedure Del (Index : in Natural := 0);

  -- Locate a word, returns 0 if not found
  function Search (Word : String) return Natural;

end Words;

