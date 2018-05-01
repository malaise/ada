-- Various utilities on strings
package Str_Tools is

  -- Convert the characters of Str into upper char
  function Upper_Str (Str : String) return String;

  -- Convert the characters of Str:
  -- Any letter that follows a letter is lower char
  -- Any other  letter (including the first letter) is UPPER char
  function Mixed_Str (Str : String) return String;

  -- Remove heading / tailing spaces and Htabs
  type Strip_Kind is (Tail, Head, Both);
  function Strip (Str : String; From : Strip_Kind := Tail) return String;

  -- Locate the Nth occurence of a fragment within a string,
  --  between a given index (first/last if 0) and the end/beginning of the
  --  string, searching forward or backward
  -- Return the index in Within of the char matching the start of Fragment
  -- Return 0 if Index not in Within, if Within or Fragment is empty,
  --  or if not found
-- Locate Nth occurence of a fragment within a string,
  --  between a given index (first/last if 0) and the end/beginning of string,
  --  searching forward or backward
  -- Returns index in Within of char matching start of Fragment
  --  or 0 if not found or if Within or Fragment is empty
  function Locate (Within     : String;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1)
           return Natural;

end Str_tools;

