with Tcp_Util;
package Sync_Mng is

  type End_Of_Sync_Callback is access procedure;

  procedure Init;

  -- Start a sync reception sequence
  procedure Start;
  -- Inform that a sync has been received
  procedure Sync_Received;


  -- Add a new slave to next sync
  procedure Send (To : Tcp_Util.Host_Name);
  
  -- Are we receiving or sending sync
  function In_Sync return Boolean;
  -- Abort a send/receive sequence
  procedure Cancel;

end Sync_Mng;

