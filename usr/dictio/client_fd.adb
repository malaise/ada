with Dynamic_List;
package body Client_Fd is

  type Client_Rec is record
    Soc : Socket.Socket_Dscr;
    Fd  : Event_Mng.File_Desc;
  end record;

  package Client_List_Mng is new Dynamic_List(Client_Rec);
  Client_List : Client_List_Mng.List_Type;

  function Fd_Match (El1, El2 : Client_Rec) return Boolean is
    use type Event_Mng.File_Desc;
  begin
    return El1.Fd = El2.Fd;
  end Fd_Match;
  procedure Search_Fd is new Client_List_Mng.Search(Fd_Match);

  function Soc_Match (El1, El2 : Client_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return El1.Soc = El2.Soc;
  end Soc_Match;
  procedure Search_Soc is new Client_List_Mng.Search(Soc_Match);

  procedure Add_Client (Client : in Socket.Socket_Dscr) is
    Rec : Client_Rec;
  begin
    Rec.Soc := Client;
    Rec.Fd := Socket.Fd_Of (Client);
    begin
      Search_Fd (Client_List, Rec, From => Client_List_Mng.Absolute);
      raise Client_Error;
    exception
      when Client_List_Mng.Not_In_List =>
        null;
    end;
    begin
      Search_Soc (Client_List, Rec, From => Client_List_Mng.Absolute);
      raise Client_Error;
    exception
      when Client_List_Mng.Not_In_List =>
        null;
    end;
    Client_List_Mng.Insert (Client_List, Rec);
  end Add_Client;

  procedure Del_Client (Client : in Socket.Socket_Dscr) is
    Rec : Client_Rec;
    use type Event_Mng.File_Desc;
  begin
    Rec.Soc := Client;
    begin
      Search_Soc (Client_List, Rec, From => Client_List_Mng.Absolute);
    exception
      when Client_List_Mng.Not_In_List =>
        raise Client_Error;
    end;
    Client_List_Mng.Read (Client_List, Rec, Client_List_Mng.Current);
    if Rec.Fd /= Socket.Fd_Of (Client) then
      raise Client_Error;
    end if;
    if Client_List_Mng.Get_Position (Client_List) = 1 then
      Client_List_Mng.Delete (Client_List, Client_List_Mng.Next);
    else
      Client_List_Mng.Delete (Client_List, Client_List_Mng.Prev);
    end if;
    Event_Mng.Del_Fd_Callback (Rec.Fd, True);
    Socket.Close (Rec.Soc);
  end Del_Client;

  procedure Del_All is
    Rec : Client_Rec;
  begin
    Client_List_Mng.Move_To (Client_List, Client_List_Mng.Next, 0, False);
    loop
      Client_List_Mng.Read (Client_List, Rec, Client_List_Mng.Current);
      Event_Mng.Del_Fd_Callback (Rec.Fd, True);
      Socket.Close (Rec.Soc);
      Client_List_Mng.Delete (Client_List);
    end loop;
  exception
    when Client_List_Mng.Empty_List | Client_List_Mng.Not_In_List =>
      Client_List_Mng.Delete_List (Client_List);
  end Del_All;


  function Socket_Of (Fd : Event_Mng.File_Desc) return Socket.Socket_Dscr is
    Rec : Client_Rec;
  begin
    Rec.Fd := Fd;
    begin
      Search_Fd (Client_List, Rec, From => Client_List_Mng.Absolute);
    exception
      when Client_List_Mng.Not_In_List =>
        raise Client_Error;
    end;
    Client_List_Mng.Read (Client_List, Rec, Client_List_Mng.Current);
    return Rec.Soc;
  end Socket_Of;

  -- Read first/next client
  -- Socket.No_Socket when no more
  procedure Read_First (Client : out Socket.Socket_Dscr) is
    Rec : Client_Rec;
  begin
    if Client_List_Mng.Is_Empty (Client_List) then
      Client := Socket.No_Socket;
      return;
    end if;
    Client_List_Mng.Move_To (Client_List, Client_List_Mng.Next, 0, False);
    Client_List_Mng.Read (Client_List, Rec, Client_List_Mng.Current);
    Client := Rec.Soc;
  end Read_First;

  procedure Read_Next  (Client : out Socket.Socket_Dscr) is
    Rec : Client_Rec;
  begin
    Client_List_Mng.Move_To (Client_List);
    Client_List_Mng.Read (Client_List, Rec, Client_List_Mng.Current);
    Client := Rec.Soc;
  exception
    when Client_List_Mng.Not_In_List =>
      Client := Socket.No_Socket;
  end Read_Next;

end Client_Fd;

