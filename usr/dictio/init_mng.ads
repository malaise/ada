with Tcp_Util;
with Status;
package Init_Mng is

  procedure Start;

  procedure Event (From : in Tcp_Util.Host_Name;
                   Stat : in Status.Status_List;
                   Diff : in Boolean);

end Init_Mng;

