with As.U.Utils;
package Analist is

  -- The maximum length supported
  -- Longer words of the dictionnary are discarded
  Max_Len : constant := 15;

  -- Init database from a dictionnary (file with one word per line)
  -- Reset it if already init
  Init_Error : exception;
  procedure Init (Words_File, Nouns_File : in String);

  -- Add a word in the database if it does not exist
  procedure Add (Word : in As.U.Asu_Us; Noun : in Boolean);
  -- Delete a word from the database if it exists
  procedure Del (Word : in As.U.Asu_Us; Noun : in Boolean);

  -- List the anagrams of Letters in the database
  Too_Long : exception;
  procedure List (Letters : in String;
                  Also_In_Nouns : in Boolean;
                  Anagrams : out As.U.Utils.Asu_Ua.Unb_Array);

end Analist;

