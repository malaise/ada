with Ada.Characters.Latin_1;
package String_Mng is

  -- Parces spaces and tabs (Latin_1.Ht) from the head/tail of a string
  -- Returns the position of the first/last character or 0 if
  --  all the string is spaces or tabs (or empty)
  function Parse_Spaces (Str : String; From_Head : Boolean := True)
           return Natural;

  -- Puts a string str in a string of fixed length Len.
  -- If Str is shorter than Len, it is aligned at right or left and padded
  -- If Str is longer  than Len, it's head ot tail is truncated
  -- Str : String to put in the returned string
  -- Len : Number of characters of the returned string
  -- Align_Left : If string is shorter than Len characters,
  --     align it at left or at right (not Align_Left) and fill with Gap,
  -- Gap : When string is shorter than Len, fill empty positions with Gap
  -- Trunc_Head : If string is longer than Len characters, trunc it's head
  --     or its tail (not Trunc_Head)
  -- Show_Trunc : When string is longer than Len, if Show_Trunc is set,
  --         then Str is truncated to Len-2 and starts (Trunc_Head) with "> "
  --         or ends (not Trunc_Head) with " <"
  function Procuste (Str : String;
                     Len : Positive;
                     Align_Left : Boolean := True;
                     Gap : Character := ' ';
                     Trunc_Head : Boolean := True;
                     Show_Trunc : Boolean := True)
           return String;

  -- Locate Nth occurence of a fragment within a string, starting
  --  at a given index and from head or tail
  -- Returns index in Within of char matching start of Fragment
  --  or 0 if not found or if Within or Fragment is empty
  function Locate (Within     : String;
                   From_Index : Positive;
                   Fragment   : String;
                   Occurence  : Positive := 1;
                   From_Head  : Boolean := True)
           return Natural;

  -- Remove Nb_Char characters from the string From at index At_Index.
  -- If Gap is No_Gap, then the string is shorten by Nb_Char, either
  --  those after At_Index if Shift_Left is True, or those before
  --  At_Index if Shift_Left is False.
  -- If Gap is set, then the string From keeps its length and is padded
  --  by Gap, either at the end (if Shift_Left is True) or at the
  --  beginning (if Shift_Left is False).
  -- Return the remaining string.
  -- Raises Constraint_Error if At_Index is not within From'First .. From'Last.
  No_Gap : constant Character :=  Ada.Characters.Latin_1.Nul;
  function Remove (From : String;
                   At_Index : Positive;
                   Nb_Char : Natural;
                   Shift_Left : Boolean := True;
                   Gap : Character := No_Gap)
           return String;

  -- If To_Right is True, extract Nb_Char characters of From from At_Index
  -- If To_Right is False, extract Nb_Char characters of From up to At_Index
  -- Return the extracted substring.
  -- Raises Constraint_Error
  --  if At_Index+Nb_Char-1 is more than From'Last  (when To_Right is True)
  --  or At_Index-Nb_Char+1 is less than From'First (when To_Right is False).
  function Slice (From : String;
                  At_Index : Positive;
                  Nb_Char : Natural;
                  To_Right : Boolean := True)
           return String;

  -- Extract the Nb_Char first (if Head is set to True) or last characters
  --   (if Head is set to False) of From string.
  -- Return the remaining string.
  function Cut (From : String;
                Nb_Char : Natural;
                Head : Boolean := True)
           return String;

  -- Extract the Nb_Char first (if Head is set to True) or last characters
  --    (if Head is set to False) of From string.
  -- Return the extracted substring.
  -- Raises Constraint_Error if Nb_Char is more than From'Length.
  function Extract (From : String;
                    Nb_Char : Natural;
                    Head : Boolean := True)
           return String;

  -- Swap the characters of string
  -- Example: ABCD -> DCBA
  function Swap (Str : String) return String;

  -- Remove any multiple occurence of a character from string.
  -- Check from head or tail and return string.
  -- Example: ABCAD, Head -> ABCD  and  ABCAD, Tail -> BCAD
  function Unique (From : String;
                   From_Head : Boolean := True)
           return String;

  -- This function returns the value of a variable. It may raise exceptions
  --  (that will be propagated by Eval_Variables).
  type Resolv_Access is access function (Variable_Name : String) return String;
  -- Replace recursively all variables by their values provided by the
  --  Resolv callback.
  -- A variable name is identified because it is within delimiters (strings).
  -- Start and stop delimiters must be non empty and different (e.g. "(" and ")",
  --  or "${" and "}"), otherwise Inv_Delimiter is raised.
  -- Variables may be defined recursively (e.g. ${Foo${Bar}}).
  -- Delimiter number must match (as many stop as start and in consistent
  --  sequence e.g. {}}{ s forbidden), otherwise the exception
  --  Delimiter_Mismatch is raised.
  -- If no callback is set (Resolv = null) then variables are replaced by
  --  empty strings.
  function Eval_Variables (Str : String;
                           Start_Delimiter, Stop_Delimiter : in String;
                           Resolv : Resolv_Access)
           return String;
  Inv_Delimiter, Delimiter_Mismatch : exception;

  -- Locate an escape sequence within the Within string,
  --  starting searching from From_Index.
  -- An escape sequence is one escape character followed by the possible
  --  escaped characters. The escape character can escape itself.
  --  (e.g. Escape="\na" will detect "\\" "\n" or "\a").
  -- Returns the index in Within of the escaped matching character
  --  (e.g. the '\', 'n' or 'a' following the first '\'), or 0 if not found.
  -- Also returns 0 if Escape is empty.
  function Locate_Escape (Within_Str : String;
                          From_Index : Positive;
                          Escape     : String) return Natural;
end String_Mng;

