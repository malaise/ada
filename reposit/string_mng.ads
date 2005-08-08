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

end String_Mng;

