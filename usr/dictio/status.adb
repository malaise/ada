with Debug;
package body Status is

  Current_Status : Status_List := Starting;
  Stable_Status : Stable_Status_List := Dead;
  Status_Cb : New_Status_Callback := null;

  procedure Set (Status : in Status_List) is
    Prev_Status : constant Status_List := Current_Status;
  begin
    if Current_Status /= Dead and then Status /= Current_Status then
      if Debug.Level_Array(Debug.Status) then
        Debug.Put ("Status: " & Prev_Status'Img & " -> " & Status'Img);
      end if;
      Current_Status := Status;
      case Current_Status is
        when Starting | Init | Dead =>
          Stable_Status := Dead;
        when Slave | Master =>
          Stable_Status := Current_Status;
        when Fight =>
          null;
      end case;
      if Status_Cb /= null then
        Status_Cb.all (Prev_Status, Current_Status);
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

  function Get_Stable return Stable_Status_List is
  begin
    return Stable_Status;
  end Get_Stable;

end Status;

