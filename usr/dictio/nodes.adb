with Dynamic_List;
with Local_Host_Name, Parse, Dictio_Debug;
package body Nodes is

  type Node_Rec is record
    Name : Tcp_Util.Host_Name;
    Stat : Status.Status_List;
    Sync : Boolean;
    Prio : Args.Prio_Str;
  end record;

  package Node_Dyn_List_Mng is new Dynamic_List(Node_Rec);
  package Node_Mng renames Node_Dyn_List_Mng.Dyn_List;

  Node_List : Node_Mng.List_Type;

  function Name_Match (El1, El2 : Node_Rec) return Boolean is
  begin
    return El1.Name = El2.Name;
  end Name_Match;

  procedure Search_Name is new Node_Mng.Search (Name_Match);


  procedure Init_List is
    N : Tcp_Util.Host_Name;
  begin
    Node_List.Delete_List (Deallocate => True);
    Local_Host_Name.Get (N);
    Set (N, Status.Get, Status.Sync, Args.Get_Prio);
  end Init_List;

  function Search_Name (Name : Tcp_Util.Host_Name) return Boolean is
    Rec : Node_Rec;
    Found : Boolean;
  begin
    Rec.Name := Name;
    Search_Name (Node_List, Found, Rec, From => Node_Mng.Absolute);
    return Found;
  end Search_Name;

  procedure Set (Name : in Tcp_Util.Host_Name;
                 Stat : in Status.Status_List;
                 Sync : in Boolean;
                 Prio : in Args.Prio_Str) is
    Rec, Grec : Node_Rec;
    Moved : Boolean;
    use type Status.Status_List;
  begin
    Rec.Name := Name;
    Rec.Stat := Stat;
    Rec.Sync := Sync;
    Rec.Prio := Prio;
    if Search_Name (Name) then
      if Stat /= Status.Dead then
        if Stat /= Status.Fight then
          -- Known and not dead and not fight => replace
          if Prio = No_Prio then
            Node_List.Read (Grec, Node_Mng.Current);
            Rec.Prio := Grec.Prio;
          end if;
          Node_List.Modify (Rec, Node_Mng.Current);
        end if;
      else
        -- Dead => delete
        Node_List.Delete (Node_Mng.Prev, Moved);
      end if;
    elsif Stat /= Status.Dead then
      -- Unknown and alive => insert
      Node_List.Rewind (False);
      Node_List.Insert (Rec);
    end if;
  end Set;

  function Less_Than (El1, El2 : Node_Rec) return Boolean is
    use type Status.Status_List;
  begin
    -- Master is better
    if El1.Stat = Status.Master and then El2.Stat /= Status.Master then
      return True;
    elsif El1.Stat /= Status.Master and then El2.Stat = Status.Master then
      return False;
    end if;

    -- None is master, Synchronised is better
    if El1.Sync /= El2.Sync then
      return El1.Sync;
    end if;

    -- Slave is better than init
    if El1.Stat = Status.Slave and then El2.Stat = Status.Init then
      return True;
    end if;

    -- Prio
    if El1.Prio /= El2.Prio then
      return El1.Prio > El2.Prio;
    end if;

    -- First name
    return El1.Name < El2.Name;
  end Less_Than;


  procedure Sort is new Node_Mng.Sort (Less_Than);

  function Check return Check_Result_List is
    Rec : Node_Rec;
    Own_Name : Tcp_Util.Host_Name;
    Will_Be_Master : Boolean;
    use type Status.Status_List;
    Result : Check_Result_List;
  begin

    -- This should not occure, but well...
    if Node_List.Is_Empty then
      return No_Master_Slave;
    end if;

    Local_Host_Name.Get (Own_Name);

    -- First is the best
    Sort (Node_List);

    -- Read first record
    Node_List.Read (Rec, Node_Mng.Current);

    -- Consider we are slave, but keep in mind if we are first
    Result := All_Init_Slave;
    Will_Be_Master := Rec.Name = Own_Name;

    loop
      if Dictio_Debug.Level_Array(Dictio_Debug.Fight) then
        Dictio_Debug.Put ("Fight.Check: " & Parse (Rec.Name)
                 & "/" & Rec.Stat'Img & "-" & Rec.Prio
                 & " Sync: " & Rec.Sync'Img);
      end if;
      if Rec.Stat = Status.Master then
        -- One master found
        if Result /= One_Master_Slave
        and then Result /= Many_Master_Slave then
          -- No master yet, got one
          Result := One_Master_Slave;
        else
          -- Already one or many masters, yet another
          Result := Many_Master_Slave;
        end if;
      end if;
      if       Rec.Stat /= Status.Starting
      and then Rec.Stat /= Status.Init then
        -- Fight or slave
        if Result = All_Init_Slave then
          -- No slave yet, got one
          Result := No_Master_Slave;
        end if;
      end if;
      -- Next
      exit when not Node_List.Check_Move;
      Node_List.Move_To;
      Node_List.Read (Rec, Node_Mng.Current);
    end loop;

    Node_List.Delete_List (Deallocate => True);

    -- Are we potential master?
    if Will_Be_Master then
      Result := Check_Result_List'Pred(Result);
    end if;
    return Result;
  end Check;

end Nodes;

