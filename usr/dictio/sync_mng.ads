with Tcp_Util;
package Sync_Mng is

  type End_Of_Sync_Callback is access procedure;

  -- Start/Abort a sync reception sequence
  procedure Start;
  procedure Cancel;
  -- Inform that a sync has been received
  procedure Sync_Received;


  -- Add a new slave to next sync
  procedure Send (To : Tcp_Util.Host_Name);
  
  -- Are we receiving or sending sync
  function In_Sync return Boolean;

end Sync_Mng;

