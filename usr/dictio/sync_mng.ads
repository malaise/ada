package Sync_Mng is

  type End_Of_Sync_Callback is access procedure;

  -- Start/Abort a sync reception sequence
  procedure Start;
  procedure Cancel;
  -- Inform that a sync has been received
  procedure Sync_Received;


  -- Send sync to slave 
  procedure Send;
  
  -- Are we receiving or sending sync
  function In_Sync return Boolean;

end Sync_Mng;

