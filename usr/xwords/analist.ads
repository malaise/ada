with As.U; use As.U;
package Analist is

  -- List the various combinations in Letters that pass Check
  -- Stop when Max words found
  procedure List (Letters : in String;
                  Result : out Asu_Ua.Unb_Array;
                  Check : access function (Word : String) 
                                          return Boolean := null;
                  Max : in Positive := 10);
end Analist;

