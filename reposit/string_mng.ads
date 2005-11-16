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
  function Procuste (Str : String; Len : Positive;
           Align_Left : Boolean := True; Gap : Character := ' ';
           Trunc_Head : Boolean := True; Show_Trunc : Boolean := True)
           return String;

  -- Locate Nth occurence of a fragment within a string
  -- Returns index of fragment start
  --  or 0 if not found or if Within or Fragment is empty
  function Locate (Within : String;
                   Fragment : String;
                   Occurence : Positive := 1) return Natural;

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
  function Remove (From : String; At_Index : Positive; Nb_Char : Natural;
                   Shift_Left : Boolean := True;
                   Gap : Character := No_Gap) return String;

  -- If To_Right is True, extract Nb_Char characters of From from At_Index
  -- If To_Right is False, extract Nb_Char characters of From up to At_Index
  -- Return the extracted substring.
  -- Raises Constraint_Error
  --  if At_Index+Nb_Char-1 is more than From'Last  (when To_Right is True)
  --  or At_Index-Nb_Char+1 is less than From'First (when To_Right is False).
  function Slice (From : String; At_Index : Positive; Nb_Char : Natural;
                  To_Right : Boolean := True) return String;

  -- Extract the Nb_Char first (if Head is set to True) or last characters
  --   (if Head is set to False) of From string.
  -- Return the remaining string.
  function Cut (From : String; Nb_Char : Natural;
                Head : Boolean := True) return String;

  -- Extract the Nb_Char first (if Head is set to True) or last characters
  --    (if Head is set to False) of From string.
  -- Return the extracted substring.
  -- Raises Constraint_Error if Nb_Char is more than From'Length.
  function Extract (From : String; Nb_Char : Natural;
                    Head : Boolean := True) return String;

  -- Swap the characters of string
  -- Example: ABCD -> DCBA
  function Swap (Str : String) return String;

  -- Remove any multiple occurence of a character from string.
  -- Check from head or tail and return string.
  -- Example: ABCAD, Head -> ABCD  and  ABCAD, Tail -> BCAD
  function Unique (From : String; From_Head : Boolean := True) return String;

  -- Replace recursively all variables by their values in environ (getenv).
  -- A variable name is identified because it is within delimiters (strings).
  -- Start and stop delimiters must be non empty and different (e.g. "(" and ")",
  --  or "${" and "}"), otherwise Inv_Delimiter is raised.
  -- Variables may be defined recursively (e.g. ${Foo${Bar}}).
  -- Delimiter number must match (as many stop as start and in consistent
  --  sequence e.g. {}}{ s forbidden), otherwise the exception
  --  Delimiter_Mismatch is raised.
  -- When a variable is not defined in environ, either it is expanded to
  --  an empty string or the exception No_Variable is raised.
  function Eval_Variables (Str : String;
                           Start_Delimiter, Stop_Delimiter : in String;
                           Raise_No_Var : in Boolean := False) return String;
  Inv_Delimiter, Delimiter_Mismatch, No_Variable : exception;

end String_Mng;

