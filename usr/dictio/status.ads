-- The status of process
package Status is

  type Status_List is (Starting, Init, Slave, Master, Dead, Fight);

  procedure Set (Status : in Status_List;
                 Immediate : in Boolean := False);

  function Get return Status_List;

  subtype Stable_Status_List is Status_List range Slave .. Dead;
  function Get_Stable return Stable_Status_List;

  type New_Status_Callback is access
           procedure (Prev_Status, New_Status : in Status_List);
  procedure Set (New_Status_Cb : in New_Status_Callback);

  Sync : Boolean := False;

end Status;

