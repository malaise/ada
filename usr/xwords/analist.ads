with As.U.Utils;
package Analist is

  -- The maximum length supported
  -- Longer words of the dictionnary are discarded
  Max_Len : constant := 15;

  -- Init database from a dictionnary (file with one word per line)
  -- Reset it if already init
  Init_Error : exception;
  procedure Init (File_Name : in String);

  -- Add a word in the database if it does not exist
  procedure Add (Word : in As.U.Asu_Us);
  -- Delete a word from the database if it exists
  procedure Del (Word : in As.U.Asu_Us);

  -- List the anagrams of Letters in the database
  Too_Long : exception;
  procedure List (Letters : in String;
                  Anagrams : out As.U.Utils.Asu_Ua.Unb_Array);

end Analist;

