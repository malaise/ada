with As.U; use As.U;
package Analist is

  -- The maximum length supported
  -- Longer words of the dictionnary are discarded
  Max_Len : constant := 15;

  -- Init database from a dictionnary (file with one word per line)
  -- Reset it if already init
  Init_Error : exception;
  procedure Init (File_Name : in String);

  -- List the anagrams of Letters in the database
  Too_Long : exception;
  procedure List (Letters : in String;
                  Anagrams : out Asu_Ua.Unb_Array);

end Analist;

