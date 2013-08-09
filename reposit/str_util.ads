-- Various String utilities
with Ada.Characters.Latin_1;
with Many_Strings;
package Str_Util is

  -- Return a String(1 .. N), copy of Str
  -- All functions returning a String return a Normalized String
  function Normalize (Str : String) return String;

  -- Swap the characters of string
  -- Example: ABCD -> DCBA
  function Swap (Str : String) return String;

  -- Remove any multiple occurence of a character from string.
  -- Check from head or tail and return string.
  -- Example: ABCAD, Head -> ABCD  and  ABCAD, Tail -> BCAD
  function Unique (From : String;
                   From_Head : Boolean := True)
           return String;

  -- Overwrite a part of a string by a new one
  -- Do nothing if New_Str is empty
  -- Append New_Item if Position = Source'Last + 1
  -- Extend Source if Position + New_Str'Length - 1 > Source'Last
  -- Raises Constraint_Error if Position < Source'First
  --                         or Position > Source'Last + 1
  function Overwrite (Source   : String;
                      Position : Positive;
                      New_Str  : String) return String;

  -- Copy the string Val at the beginning of the string To
  -- To (To'First .. To'First + Val'Length - 1) := Val;
  procedure Copy (Val : in String; To : in out String);


  -- Replace a slice by a new string
  -- Delete chars if By is empty (except if High < Low)
  -- Insert By before Low if High < Low
  -- Append By if Low = Source'Last + 1 (and High < Low)
  -- Raises Constraint_Error if Low < Source'First
  --                         or Low > Source'Last + 1 or High > Source'Last
  function Replace (Source   : String;
                    Low      : Positive;
                    High     : Natural;
                    By       : String) return String;

  -- Insert a string before a given position
  -- Append if Before = Source'Last + 1
  -- Raises Constraint_Error if Before < Source'First
  --                         or Before > Source'Last + 1
  function Insert (Source  : String;
                   Before  : Positive;
                   New_Str : String) return String;

  -- Delete some characters
  -- Do nothing if Through < From
  -- Raises Constraint_Error if Through >= From and
  --  From < Source'First or From > Source'Last
  --  or Through > Source'Last
  function Delete (Source  : String;
                   From    : Positive;
                   Through : Natural) return String;

  -- Delete Number characters from From included
  --  or as many characters as possible
  -- May raise Constraint_Error if From > Source.Length
  function Delete_Nb (Source : String;
                      From   : Positive;
                      Number : Natural) return String;

  -- Remove heading / tailing spaces and Htabs
  type Strip_Kind is (Tail, Head, Both);
  function Strip (Str : String; From : Strip_Kind := Tail) return String;

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

  -- Remove the Nb_Char first (if Head is set to True) or last
  --   (if Head is set to False) characters of From string.
  -- Return the remaining string.
  function Cut (From : String;
                Nb_Char : Natural;
                Head : Boolean := True)
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
  --    (if Head is set to False) of From string.
  -- Return the extracted substring.
  -- Raises Constraint_Error if Nb_Char is more than From'Length.
  function Extract (From : String;
                    Nb_Char : Natural;
                    Head : Boolean := True)
           return String;

  -- Puts the string Str in a string of fixed length Len.
  -- If Str is shorter than Len, it is aligned at right or left and padded
  -- If Str is longer  than Len, it's head or tail is truncated
  -- Str : String to put in the returned string
  -- Len : Number of characters of the returned string
  -- Align_Left : If string is shorter than Len characters,
  --     align it at left (Align_Left) or at right (not Align_Left) and fill
  --     with Gap,
  -- Gap : When string is shorter than Len, fill empty positions with Gap
  -- Trunc_Head : If string is longer than Len characters, trunc it's head
  --     (Trunc_Head) or its tail (not Trunc_Head)
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

  -- Try to cut Str so that it best matches the requested line Length
  -- Looks for separator character
  type Separator_Access is access function (Char : Character) return Boolean;
  -- Default Separator function, True for Space and Latin.Ht.
  function Is_Separator (Char : Character) return Boolean;
  -- If Str is shorter or equal to Length, return Str
  -- Else try to find a separator, successively:
  --   before Length, up to Mini
  --   after  Length, up to Maxi
  --   before Mini,   up to 1
  --   after  Maxi,   up to Str'Length
  -- If a separator is found then return Str up to this separator included,
  -- Else return Str
  -- Prerequisits Mini <= Length <= Maxi (else Constraint_Error).
  -- Beware that Mini, Maxi and Length are not relative to Str
  -- Returns "" only if Str is empty.
  function Truncate (Str : String;
                     Length : Positive;
                     Mini, Maxi : Positive;
                     Separating : access
    function (Char : Character) return Boolean := Is_Separator'Access)
  return String;

  -- Center a String Str in a fixed size
  -- if Str <= Size pad with Gap before then after Str
  -- if Str > Size  raise Constraint_Error
  -- Example: Center ("TOTO", 7, '+') -> "++TOTO+"
  function Center (Str : String;
                   Len : Positive;
                   Gap : Character := ' ') return String;

  -- Locate the Nth occurence of a fragment within a string,
  --  between a given index (first/last if 0) and the end/beginning of the
  --  string, searching forward or backward
  -- Returns the index in Within of the char matching the start of Fragment,
  --  or 0 if not found or if Within or Fragment is empty
  function Locate (Within     : String;
                   Fragment   : String;
                   From_Index : Natural := 0;
                   Forward    : Boolean := True;
                   Occurence  : Positive := 1)
           return Natural;

  -- This function returns the value of a variable. It may raise exceptions
  --  (that will be propagated by Eval_Variables).
  type Resolv_Access is access function (Variable_Name : String) return String;
  -- Replace all variables by their values provided by the Resolv callback.
  -- A variable name is identified when it is within delimiters (strings).
  -- Start and stop delimiters must be non empty and different (e.g. "(" and ")",
  --  or "${" and "}"), otherwise Inv_Delimiter is raised.
  -- Variables may be defined recursively (e.g. ${Foo${Bar}}).
  -- Delimiter number must match (as many stop as start and in consistent
  --  sequence e.g. {}}{ is forbidden), otherwise the exception
  --  Delimiter_Mismatch is raised.
  -- On option Recursive, loops re-avaluating as long as possible (otherwise
  --  only one pass)
  -- On option No_Check_Stop, extra stops are accepted ({}} is OK)
  -- If no callback is set (Resolv = null) then variables are replaced by
  --  empty strings.
  function Eval_Variables (Str : String;
                           Start_Delimiter, Stop_Delimiter : in String;
                           Resolv : access
    function (Variable_Name : String) return String;
                           Muliple_Passes : Boolean;
                           No_Check_Stop : Boolean;
                           Skip_Backslashed : Boolean)
           return String;
  Inv_Delimiter, Delimiter_Mismatch : exception;

  -- Locate an escape sequence within the Within_Str string,
  --  starting searching from From_Index.
  -- An escape sequence is one escape character followed by the possible
  --  escaped characters. The escape character can escape itself.
  --  (e.g. Escape="\na" will detect "\\" "\n" or "\a").
  -- Returns the index in Within_Str of the escaped matching character
  --  (e.g. the '\', 'n' or 'a' following the first '\'), or 0 if not found.
  -- Also returns 0 if Escape is empty.
  function Locate_Escape (Within_Str : String;
                          From_Index : Positive;
                          Escape     : String) return Natural;

  -- Check if the character at Index of Str is backslashed
  --  (the number of '\' immediately before it is odd)
  -- Raises Constraint_Error if Index is out of Str
  function Is_Backslashed (Str   : String;
                           Index : Positive) return Boolean;

  -- Split Str according to Separator
  -- Replaces Separator by Many_String.Separator but skips
  --  '\' & Separator (using Is_Backslashed),
  --  then replaces '\' & Separator by Separator
  function Split (Str       : String;
                  Separator : Character) return Many_Strings.Many_String;


  -- Replace occurences of What by By in Str. One pass.
  -- If Skip_Backslashed, use Is_Backslash to detect \What and skip it
  function Substit (Str, What, By : String;
                    Skip_Backslashed : Boolean := False) return String;

end Str_Util;

