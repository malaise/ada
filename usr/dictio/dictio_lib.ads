with Data_Base;
package Dictio_Lib is

  -- Item is (Name, Data);
  -- Name has to be an identifier:
  --  <letter> [ { [ '_' ] <letter_or_digit> } ]
  Max_Name_Len : constant := Data_Base.Item_Name'Length;
  Max_Data_Len : constant := Data_Base.Item_Data'Length;

  -- On any call, if not init or unavailable
  No_Dictio : exception;

  -- On Get/Set/Notify
  Invalid_Name, Name_Too_Long, Data_Too_Long : exception;

  -- On Get
  No_Item : exception;

  -- Callback called when dictio status changes
  type Dictio_State_List is (Master, Slave, Unavailable);
  type Dictio_State_Callback is
       access procedure (State : in Dictio_State_List);
  Dictio_State_Cb : Dictio_State_Callback := null;

  -- Callback called on notification
  type Dictio_Notification_Callback is
       access procedure (Name : in String; Data : in String);
  Notify_Cb : Dictio_Notification_Callback := null;

  -- Init connection to Dictio
  -- May raise No_Dictio if init fails
  procedure Init;

  -- Get Item data
  -- May raise Name_Too_Long or No_Item 
  function Get (Name : in String) return String;

  -- Create/Modify Item
  -- May raise Name_Too_Long or Data_Too_Long
  procedure Set (Name : in String; Data : in String);

  -- (Un)Notify on Item name
  -- May raise Name_Too_Long
  procedure Notify (Name : in String; On : in Boolean);

  -- Add/del a new host to destination list
  -- Use with care:
  --  For adding a host:
  --   insert it in the channel files of all dictios
  --   add it on all running dictios (with Add_Host)
  --   start it
  --  For deleting a host:
  --   stop it
  --   delete it from the channel files of all dictios
  --   delete it on all running dictios (with Del_Host)
  -- May raise Data_Too_Long
  procedure Add_Host (Host : in String);
  procedure Del_Host (Host : in String);

end Dictio_Lib;

