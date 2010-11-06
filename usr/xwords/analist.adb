-- with As.U; 
-- use As.U;

package body Analist is


  -- List the various combinations in Letters that pass Check
  -- Stop when Max words found
  procedure List (Letters : in String;
                  Result : out Asu_Ua.Unb_Array;
                  Check : access function (Word : String) return Boolean := null;
                  Max : in Positive := 10) is
  begin
    Result := Asu_Ua.Null_Unb_Array;
  end List;

end Analist;

