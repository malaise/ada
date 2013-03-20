with Dynamic_List;
package body Client_Fd is

  type Client_Rec is record
    Soc : Socket.Socket_Dscr;
    Fd  : Event_Mng.File_Desc;
  end record;

  package Client_Dyn_List_Mng is new Dynamic_List(Client_Rec);
  package Client_List_Mng renames Client_Dyn_List_Mng.Dyn_List;
  Client_List : Client_List_Mng.List_Type;

  function Fd_Match (El1, El2 : Client_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return El1.Fd = El2.Fd;
  end Fd_Match;
  function Search_Fd is new Client_List_Mng.Search(Fd_Match);

  function Soc_Match (El1, El2 : Client_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return El1.Soc = El2.Soc;
  end Soc_Match;
  function Search_Soc is new Client_List_Mng.Search(Soc_Match);

  procedure Add_Client (Client : in Socket.Socket_Dscr) is
    Rec : Client_Rec;
  begin
    Rec.Soc := Client;
    Rec.Fd := Client.Get_Fd;
    if Search_Fd (Client_List, Rec, From => Client_List_Mng.Absolute) then
      raise Client_Error;
    end if;
    if Search_Soc (Client_List, Rec, From => Client_List_Mng.Absolute) then
      raise Client_Error;
    end if;
    Client_List.Insert (Rec);
  end Add_Client;

  procedure Del_Client (Client : in Socket.Socket_Dscr) is
    Rec : Client_Rec;
    Moved : Boolean;
    use type Event_Mng.File_Desc;
  begin
    Rec.Soc := Client;
    if not Search_Soc (Client_List, Rec, From => Client_List_Mng.Absolute) then
      raise Client_Error;
    end if;

    Client_List.Read (Rec, Client_List_Mng.Current);
    if Rec.Fd /= Client.Get_Fd then
      raise Client_Error;
    end if;
    Client_List.Delete (Moved => Moved);
    Event_Mng.Del_Fd_Callback (Rec.Fd, True);
    Rec.Soc.Close;
  end Del_Client;

  procedure Del_All is
    Rec : Client_Rec;
  begin
    if Client_List.Is_Empty then
      return;
    end if;

    Client_List.Rewind;
    loop
      Client_List.Read (Rec, Client_List_Mng.Current);
      Event_Mng.Del_Fd_Callback (Rec.Fd, True);
      Rec.Soc.Close;
      Client_List.Delete;
      exit when Client_List.Is_Empty;
    end loop;
  end Del_All;


  function Socket_Of (Fd : Event_Mng.File_Desc) return Socket.Socket_Dscr is
    Rec : Client_Rec;
  begin
    Rec.Fd := Fd;
    if not Search_Fd (Client_List, Rec, From => Client_List_Mng.Absolute) then
      raise Client_Error;
    end if;
    Client_List.Read (Rec, Client_List_Mng.Current);
    return Rec.Soc;
  end Socket_Of;

  -- Read first/next client
  -- Socket.No_Socket when no more
  procedure Read_First (Client : out Socket.Socket_Dscr) is
    Rec : Client_Rec;
  begin
    if Client_List.Is_Empty then
      Client := Socket.No_Socket;
      return;
    end if;
    Client_List.Rewind;
    Client_List.Read (Rec, Client_List_Mng.Current);
    Client := Rec.Soc;
  end Read_First;

  procedure Read_Next  (Client : out Socket.Socket_Dscr) is
    Rec : Client_Rec;
  begin
    if not Client_List.Check_Move then
      Client := Socket.No_Socket;
      return;
    end if;
    Client_List.Move_To;
    Client_List.Read (Rec, Client_List_Mng.Current);
    Client := Rec.Soc;
  end Read_Next;

end Client_Fd;

