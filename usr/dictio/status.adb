with Debug;
package body Status is

  Current_Status : Status_List := Starting;
  Status_Cb : New_Status_Callback := null;

  procedure Set (Status : in Status_List) is
    Prev_Status : constant Status_List := Current_Status;
  begin
    if Status /= Current_Status then
      if Debug.Level_Array(Debug.Status) then
        Debug.Put ("Status: " & Prev_Status'Img & " -> " & Status'Img);
      end if;
      Current_Status := Status;
      if Status_Cb /= null then
        Status_Cb.all;
      end if;
    end if;
  end Set;

  function Get return Status_List is
  begin
    return Current_Status;
  end Get;

  procedure Set (New_Status_Cb : in New_Status_Callback) is
  begin
    Status_Cb := New_Status_Cb;
  end Set;

end Status;

