-- The status of process
package Status is

  type Status_List is (Starting, Init, Slave, Master, Fight, Dead);

  procedure Set (Status : in Status_List);

  function Get return Status_List;


  type New_Status_Callback is access procedure;
  procedure Set (New_Status_Cb : in New_Status_Callback);

end Status;

