with Tcp_Util;
with Status;
package Online_Mng is

  -- Period of master sending alive message
  Alive_Period : constant Duration := 1.0;

  procedure Start (First : in Boolean);

  procedure Event (From  : in Tcp_Util.Host_Name;
                   Stat  : in Status.Status_List;
                   Sync  : in Boolean;
                   Diff  : in Boolean;
                   Extra : in String := ""); 

end Online_Mng;

