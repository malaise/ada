with Data_Base;
package Dictio_Lib is

  -- Item is (Name, Data);
  Max_Name_Len : constant := Data_Base.Item_Name'Length;
  Max_Data_Len : constant := Data_Base.Item_Data'Length;

  -- On any call
  No_Dictio : exception;

  -- On Get/Set/Notify
  Name_Too_Long, Data_Too_Long : exception;

  -- On Get
  No_Item : exception;

  -- Callback called when dictio availability changes
  type Dictio_Availability_Callback is
       access procedure (Available : in Boolean);
  Available_Cb : Dictio_Availability_Callback := null;

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

end Dictio_Lib;

