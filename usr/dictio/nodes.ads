with Tcp_Util;
with Status;
package Nodes is

  procedure Init_List;

  procedure Set (Name : in Tcp_Util.Host_Name; Stat : in Status.Status_list);

  type Check_Result_List is (             -- Are we potential master and:
   One_Master_Master, One_Master_Slave,   -- One master found
   Many_Master_Master, Many_Master_Slave, -- Several masters found
   All_Init_Master, All_Init_Slave,       -- All in init
   No_Master_Master, No_Master_Slave);    -- No master found

  function Check return Check_Result_List;

  function Better_Master_Than (Brother: Tcp_Util.Host_Name) return Boolean;

end Nodes;

