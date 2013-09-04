with Dynamic_List, Event_Mng;
with Client_Com, Client_Fd, Dictio_Debug, Parse, Names;
package body Notify is

  type Notif_Rec is record
    Client : Socket.Socket_Dscr := Socket.No_Socket;
    Item : Data_Base.Item_Name := (others => ' ');
    Kind : Data_Base.Item_Kind := Data_Base.Data_Kind;
  end record;
  package Notif_Dyn_List_Mng is new Dynamic_List (Notif_Rec);
  package Notif_List_Mng renames Notif_Dyn_List_Mng.Dyn_List;
  Notif_List : Notif_List_Mng.List_Type;

  function Full_Match (Elt1, Elt2 : Notif_Rec) return Boolean is
  begin
    return Elt1 = Elt2;
  end Full_Match;
  function Full_Search is new Notif_List_Mng.Search (Full_Match);

  function Client_Match (Elt1, Elt2 : Notif_Rec) return Boolean is
    use type Socket.Socket_Dscr;
  begin
    return Elt1.Client = Elt2.Client;
  end Client_Match;
  function Client_Search is new Notif_List_Mng.Search (Client_Match);

  function Item_Match (Elt1, Elt2 : Notif_Rec) return Boolean is
  begin
    -- Here we search in criteria list
    -- Elt1 is the notification criteria, Elt2 is the item name
    return Elt2.Kind = Elt1.Kind
    and then Names.Match (Parse (Elt2.Item), Parse (Elt1.Item));
  end Item_Match;
  function Item_Search is new Notif_List_Mng.Search (Item_Match);


  procedure Add (Client : in Socket.Socket_Dscr;
                 Item   : in Data_Base.Item_Name;
                 Kind   : in Data_Base.Item_Kind) is
  begin
    Dictio_Debug.Put (Dictio_Debug.Client_Notify, "Add: " & Parse(Item)
               & " kind " & Kind
               & " on " & Event_Mng.File_Desc'Image(Client.Get_Fd));
    Notif_List.Insert ((Client, Item, Kind));
  end Add;


  procedure Delete_Current is
    Moved : Boolean;
  begin
    Notif_List.Delete (Moved => Moved);
  end Delete_Current;


  procedure Del (Client : in Socket.Socket_Dscr;
                 Item   : in Data_Base.Item_Name;
                 Kind   : in Data_Base.Item_Kind) is
    Rec : Notif_Rec;
  begin
    Rec := (Client, Item, Kind);
    if not Full_Search (Notif_List, Rec, From => Notif_List_Mng.Absolute) then
      Dictio_Debug.Put (Dictio_Debug.Client_Notify, "Del: not found "
             & Parse(Item)
             & " kind " & Kind
             & " on " & Event_Mng.File_Desc'Image(Client.Get_Fd));
      return;
    end if;
    Dictio_Debug.Put (Dictio_Debug.Client_Notify, "Del: " & Parse(Item)
               & " kind " & Kind
               & " on " & Event_Mng.File_Desc'Image(Client.Get_Fd));
    Delete_Current;
  end Del;


  procedure Del_Client (Client : in Socket.Socket_Dscr) is
    Rec : Notif_Rec;
  begin
    Rec.Client := Client;
    if not Client_Search (Notif_List, Rec, From => Notif_List_Mng.Absolute) then
      -- No notification
      return;
    end if;
    loop
      Notif_List.Read (Rec, Notif_List_Mng.Current);
      Dictio_Debug.Put (Dictio_Debug.Client_Notify, "Del_client: "
               & Parse(Rec.Item)
               & " kind " & Rec.Kind
               & " on " & Event_Mng.File_Desc'Image(Rec.Client.Get_Fd));
      Delete_Current;
      exit when not Client_Search (Notif_List, Rec,
                                   From => Notif_List_Mng.From_Current);
    end loop;
    -- No more notification
  end Del_Client;


  procedure Del_All is
  begin
    Dictio_Debug.Put (Dictio_Debug.Client_Notify, "Del_all");
    Notif_List.Delete_List;
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
    Rec.Kind := Item.Kind;
    if not Item_Search (Notif_List, Rec, From => Notif_List_Mng.Absolute) then
      -- No notifcation
      return;
    end if;
    loop
      Notif_List.Read (Rec, Notif_List_Mng.Current);
      Fd := Rec.Client.Get_Fd;
      declare
        Dummy : Boolean;
        pragma Unreferenced (Dummy);
      begin
        Dummy := Client_Com.Dictio_Send (Rec.Client, null, null, 0.0, Msg);
        Dictio_Debug.Put (Dictio_Debug.Client_Notify, "Send: "
                   &  Parse(Item.Name)
                   & " kind " & Item.Kind
                   & " on " & Fd'Img);
      exception
        when Socket.Soc_Tail_Err =>
          null;
        when Socket.Soc_Conn_Lost =>
          Dictio_Debug.Put (Dictio_Debug.Client_Notify,
                            "Send: lost connection with " & Fd'Img);
          Del_Client (Rec.Client);
          Client_Fd.Del_Client (Rec.Client);
      end;
      -- Search next
      exit when not Item_Search (Notif_List, Rec,
                                 From => Notif_List_Mng.Skip_Current);
    end loop;
    -- No more notification
  end Send;

end Notify;

