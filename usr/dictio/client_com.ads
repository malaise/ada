with Socket, Tcp_Util;
with Data_Base;
package Client_Com is

  type Client_Action_List is (Version, State, Get, Set, Notif_On, Notif_Off,
                              Add_Host, Del_Host);

  type Dictio_Client_Rec is record
    Action : Client_Action_List;
    Item : Data_Base.Item_Rec;
  end record;
    
  procedure Dictio_Receive is new Socket.Receive (Dictio_Client_Rec);

  function Dictio_Send is new Tcp_Util.Send (Dictio_Client_Rec);

end Client_Com;

