with Ada.Characters.Latin_1;
with Tcp_Util;
with Data_Base, Status;
package Intra_Dictio is

  Stat_Kind : constant Character := Ada.Characters.Latin_1.Syn;
  Data_Kind : constant Character := Ada.Characters.Latin_1.Can;
  Sync_Kind : constant Character := Ada.Characters.Latin_1.Ack;

  type Read_Cb_Access is access procedure (
    Diff : in Boolean;
    Stat : in Status.Status_List;
    From : in Tcp_Util.Host_Name;
    Kind : in Character;
    Item : in Data_Base.Item_Rec);
  procedure Set_Read_Cb (Read_Cb : Read_Cb_Access);

  procedure Init;
  procedure Quit;

  procedure Send_Status (Extra : in String := "");
  procedure Reply_Status (Extra : in String := "");

  procedure Send_Data (Item : in Data_Base.Item_Rec);

  type Reply_Result_List is (Ok, Overflow, Error);
  function Reply_Sync_Data (Item : in Data_Base.Item_Rec)
                            return Reply_Result_List;

end Intra_Dictio;

