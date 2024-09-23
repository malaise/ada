with As.U.Utils;
package Analist is

  -- The maximum length supported for anagrams
  Max_Len : constant := 15;
  Too_Long : exception;

  -- List the anagrams of Letters in the database
  procedure List (Letters : in String;
                  In_Nouns : in Boolean;
                  Anagrams : out As.U.Utils.Asu_Ua.Unb_Array);

end Analist;

