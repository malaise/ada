with Tcp_Util;
with Args, Status, Nodes;
package Fight_Mng is

  type Fight_Action is array (Nodes.Check_Result_List) of Status.Status_List;

  procedure Start (New_Status : in Status.Status_List;
                   Timeout : in Duration;
                   Actions : in Fight_Action);

  function In_Fight return Boolean;

  procedure Event (From : in Tcp_Util.Host_Name;
                   Stat : in Status.Status_List;
                   Sync : in Boolean;
                   Prio : in Args.Prio_Str;
                   Diff : in Boolean;
                   Extra : in String := ""); 

end Fight_Mng;

