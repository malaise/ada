with Dynamic_List;
with Local_Host_Name;
package body Nodes is
  type Node_Rec is record
    Name : Tcp_Util.Host_Name;
    Stat : Status.Status_list;
  end record;

  package Node_Mng is new Dynamic_List(Node_Rec);

  Node_List : Node_Mng.List_Type;

  function Name_Match (El1, El2 : Node_Rec) return Boolean is
  begin
    return El1.Name = El2.Name;
  end Name_Match;
  
  procedure Search_Name is new Node_Mng.Search (Name_Match);


  procedure Init_List is
    N : Tcp_Util.Host_Name;
  begin
    Node_Mng.Delete_List (Node_List, Deallocate => True);
    Local_Host_Name.Get (N);
    Set (N, Status.Get);
  end Init_List;

  function Search_Name (Name : Tcp_Util.Host_Name) return Boolean is
    Rec : Node_Rec;
  begin
    Rec.Name := Name;
    Search_Name (Node_List, Rec, From_Current => False);
    return True;
  exception
    when Node_Mng.Not_In_List =>
      return False;
  end Search_Name;

  function Get_Current_Status return Status.Status_list is
    Rec : Node_Rec;
  begin
    Node_Mng.Read (Node_List, Rec, Node_Mng.Current);
    return Rec.Stat;
  end Get_Current_Status;

  procedure Set (Name : in Tcp_Util.Host_Name; Stat : in Status.Status_list) is
    Rec : Node_Rec;
    use type Status.Status_List;
  begin
    Rec.Name := Name;
    Rec.Stat := Stat;
    if Search_Name (Name) then
      if Stat /= Status.Dead then
        Node_Mng.Modify (Node_List, Rec, Node_Mng.Current);
      else
        if Node_Mng.Get_Position (Node_List) = 1 then
          Node_Mng.Delete (Node_List, Node_Mng.Next);
        else
          Node_Mng.Delete (Node_List, Node_Mng.Prev);
        end if;
      end if;
    elsif Stat /= Status.Dead then
      if not Node_Mng.is_Empty (Node_List) then
        Node_Mng.Move_To (Node_List, Node_Mng.Next, 0, False);
      end if;
      Node_Mng.Insert (Node_List, Rec);
    end if;
  end Set;

  function Better_Master_Than (Brother: Tcp_Util.Host_Name) return Boolean is
    Me : Tcp_Util.Host_Name;
  begin
    Local_Host_Name.Get (Me);
    return Me < Brother;
  end Better_Master_Than;

  function Less_Than (El1, El2 : Node_Rec) return Boolean is
  begin
    return El1.Name < El2.Name;
  end Less_Than;


  procedure Sort is new Node_Mng.Sort (Less_Than);
    
  function Check return Check_Result_List is
    Rec : Node_Rec;
    Own_Name : Tcp_Util.Host_Name;
    use type Status.Status_List;
    Result : Check_Result_List;
    Master : Boolean;
  begin

    Sort (Node_List);
    Local_Host_Name.Get (Own_Name);
    Node_Mng.Read (Node_List, Rec, Node_Mng.Current);
    Master := Rec.Name = Own_Name;
    Result := All_Init_Slave;

    loop
      if Rec.Stat = Status.Master then
        if Result /= One_Master_Slave
        and then Result /= Many_Master_Slave then
          Result := One_Master_Slave;
        else
          Result := Many_Master_Slave;
        end if;
      end if;
      if       Rec.Stat /= Status.Starting
      and then Rec.Stat /= Status.Init then
        if Result = All_Init_Slave then
          Result := No_Master_Slave;
        end if;
      end if;
      exit when Node_Mng.Get_Position (Node_List)
              = Node_Mng.List_Length (Node_List);
      Node_Mng.Move_To (Node_List);
      Node_Mng.Read (Node_List, Rec, Node_Mng.Current);
    end loop;

    Node_Mng.Delete_List (Node_List, Deallocate => True);
    if Master then
      Result := Check_Result_List'Pred(Result);
    end if;
    return Result;
  end Check;

end Nodes;

