with Tcp_Util;
with Status;
package Init_Mng is

  procedure Start;

  procedure Event (From : in Tcp_Util.Host_Name;
                   Stat : in Status.Status_List;
                   Sync : in Boolean;
                   Diff : in Boolean;
                   Extra : in String := "");

end Init_Mng;

