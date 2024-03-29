with Ada.Characters.Latin_1;
with Socket_Util;
with Args, Data_Base, Status;
package Intra_Dictio is

  -- Kind of message
  Stat_Kind : Character renames Ada.Characters.Latin_1.Enq;
  Data_Kind : Character renames Ada.Characters.Latin_1.Can;
  Sync_Kind : Character renames Ada.Characters.Latin_1.Syn;

  Extra_Crc : constant Character := 'C';
  Extra_Ver : constant Character := 'V';

  -- All characters between Key and next key (or end of Str) in Str
  function Extra_Of (Str : String; Key : Character) return String;

  type Read_Cb_Access is access procedure (
    Diff : in Boolean;
    Stat : in Status.Status_List;
    Sync : in Boolean;
    Prio : in Args.Prio_Str;
    From : in Socket_Util.Host_Name;
    Kind : in Character;
    Item : in Data_Base.Item_Rec);
  procedure Set_Read_Cb (Read_Cb : Read_Cb_Access);

  procedure Init;
  procedure Quit;

  procedure Send_Status (Stat  : in Status.Status_List;
                         Extra : in String := "");

  procedure Send_Status (Extra : in String := "");
  procedure Reply_Status (Extra : in String := "");

  procedure Send_Data (Item : in Data_Base.Item_Rec);

  type Reply_Result_List is (Ok, Overflow, Error);
  function Send_Sync_Data (To : in String;
                           Item : in Data_Base.Item_Rec)
           return Reply_Result_List;

end Intra_Dictio;

