with Dynamic_List, Socket, Event_Mng;
with Client_Com, Client_Fd, Debug, Parse, Names;
package body Notify is

  type Notif_Rec is record
    Client : Socket.Socket_Dscr := Socket.No_Socket;
    Item : Data_Base.Item_Name := (others => ' ');
  end record;
  package Notif_List_Mng is new Dynamic_List (Notif_Rec);
  Notif_List : Notif_List_Mng.List_Type;

  function Full_Match (Elt1, Elt2 : Notif_Rec) return Boolean is
  begin
    return Elt1 = Elt2;
  end Full_Match;
  procedure Full_Search is new Notif_List_Mng.Search (Full_Match);

  function Client_Match (Elt1, Elt2 : Notif_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return Elt1.Client = Elt2.Client;
  end Client_Match;
  procedure Client_Search is new Notif_List_Mng.Search (Client_Match);

  function Item_Match (Elt1, Elt2 : Notif_Rec) return Boolean is
  begin
    -- Here we search in criteria list
    -- Elt1 is the notification criteria, Elt2 is the item name
    return Names.Match (Parse (Elt2.Item), Parse (Elt1.Item));
  end Item_Match;
  procedure Item_Search is new Notif_List_Mng.Search (Item_Match);


  procedure Add (Client : in Socket.Socket_Dscr;
                 Item   : in Data_Base.Item_Name) is
  begin
    if Debug.Level_Array(Debug.Client_Notify) then
      Debug.Put ("Client-notify.add: " & Parse(Item)
               & " on " & Event_Mng.File_Desc'Image(Socket.Fd_Of(Client)));
    end if;
    Notif_List_Mng.Insert (Notif_List, (Client, Item));
  end Add;


  procedure Delete_Current is
  begin
    if Notif_List_Mng.Get_Position (Notif_List) = 1 then
      Notif_List_Mng.Delete (Notif_List, Notif_List_Mng.Next);
    else
      Notif_List_Mng.Delete (Notif_List, Notif_List_Mng.Prev);
    end if;
  end Delete_Current;


  procedure Del (Client : in Socket.Socket_Dscr;
                 Item : in Data_Base.Item_Name) is
    Rec : Notif_Rec;
  begin
    Rec := (Client, Item);
    begin
      Full_Search (Notif_List, Rec, From_Current => False);
    exception
      when Notif_List_Mng.Not_In_List =>
        if Debug.Level_Array(Debug.Client_Notify) then
          Debug.Put ("Client-notify.del: not found " & Parse(Item)
                   & " on " & Event_Mng.File_Desc'Image(Socket.Fd_Of(Client)));
        end if;
        return;
    end;
    if Debug.Level_Array(Debug.Client_Notify) then
      Debug.Put ("Client-notify.del: " & Parse(Item)
               & " on " & Event_Mng.File_Desc'Image(Socket.Fd_Of(Client)));
    end if;
    Delete_Current;
  end Del;


  procedure Del_Client (Client : in Socket.Socket_Dscr) is
    Rec : Notif_Rec;
  begin
    Rec.Client := Client;
    Client_Search (Notif_List, Rec, From_Current => False);
    loop
      if Debug.Level_Array(Debug.Client_Notify) then
        Notif_List_Mng.Read (Notif_List, Rec, Notif_List_Mng.Current);
        Debug.Put ("Client-notify.del_client: " & Parse(Rec.Item)
                 & " on " & Event_Mng.File_Desc'Image(Socket.Fd_Of(Rec.Client)));
      end if;
      Delete_Current;
      Client_Search (Notif_List, Rec);
    end loop;
  exception
    when Notif_List_Mng.Not_In_List =>
      null;
  end Del_Client;


  procedure Del_All is
  begin
    if Debug.Level_Array(Debug.Client_Notify) then
      Debug.Put ("Client-notify.del_all");
    end if;
    Notif_List_Mng.Delete_List (Notif_List);
  end Del_All;


  procedure Send (Item : in Data_Base.Item_Rec) is
    Rec : Notif_Rec;
    Msg : Client_Com.Dictio_Client_Rec;
    Fd : Event_Mng.File_Desc;
  begin
    Msg.Action := Client_Com.Notif_On;
    Msg.Item := Item;
    -- Search first notification record
    Rec.Item := Item.Name;
    Item_Search (Notif_List, Rec, From_Current => False);
    loop
      Notif_List_Mng.Read (Notif_List, Rec, Notif_List_Mng.Current);
      Fd := Socket.Fd_Of (Rec.Client);
      declare
        Dummy : Boolean;
      begin
        Dummy := Client_Com.Dictio_Send (Rec.Client, null, Msg);
        if Debug.Level_Array(Debug.Client_Notify) then
          Debug.Put ("Client-notify.send: " &  Parse(Item.Name) & " on " & Fd'Img);
        end if;
      exception
        when Socket.Soc_Tail_Err =>
          null;
        when Socket.Soc_Conn_Lost =>
          if Debug.Level_Array(Debug.Client) then
            Debug.Put ("Client-notify.send: lost connection with " & Fd'Img);
          end if;
          Del_Client (Rec.Client);
          Client_Fd.Del_Client (Rec.Client);
      end;
      -- Search next
      Notif_List_Mng.Move_To (Notif_List);
      Rec.Item := Item.Name;
      Item_Search (Notif_List, Rec, From_Current => True);
    end loop;
  exception
    when Notif_List_Mng.Not_In_List =>
      null;
  end Send;

end Notify;

