with Socket;
with Data_Base;
package Notify is

  procedure Add (Client : in Socket.Socket_Dscr;
                 Item   : in Data_Base.Item_Name);
  procedure Del (Client : in Socket.Socket_Dscr;
                 Item   : in Data_Base.Item_Name);

  procedure Del_Client (Client : in Socket.Socket_Dscr);

  procedure Del_All;

  procedure Send (Item : in Data_Base.Item_Rec);

end Notify;

