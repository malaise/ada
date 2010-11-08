with As.U; use As.U;
with File_Hash;
package Analist is

  -- The result: list of words of each lenght
  -- From Letters'Length to Min included
  Result : array (1 .. File_Hash.Max_Str_Len) of Asu_Ua.Unb_Array;

  -- List the various combinations in Letters that pass Check
  -- Discard words of length below Min
  type Check_Access is access function (Word : String) return Boolean;

  procedure List (Letters : in String;
                  Check : in Check_Access := null;
                  Min : in Positive := 1);
end Analist;

