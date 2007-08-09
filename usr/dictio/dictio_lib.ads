with Data_Base;
package Dictio_Lib is

  -- Item is (Name, Data);
  -- Name has to be of the form:  <ident> [ { .<ident> } ]
  --  where <ident> ::= <letter> [ { [ '_' ] <letter_or_digit> } ]
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
  Dictio_State_Cb : access procedure (State : in Dictio_State_List) := null;

  -- Callback called on notification (item or alias)
  Notify_Cb : access procedure (Name : in String;
                         Item : in Boolean;
                         Data : in String) := null;


  -- Init connection to Dictio
  -- May raise No_Dictio if init fails
  procedure Init;


  -- Check item name validity
  --   <ident> [ { .<ident> } ]
  function Is_Valid_Item_Name (Name : in String) return Boolean;


  -- Get Item data
  -- Syntax of name for Get/Set
  --   <ident> [ { .<ident> } ]
  -- May raise Invalid_Name, Name_Too_Long or No_Item
  function Get (Name : in String) return String;


  -- Create/Modify Item
  -- Syntax of name for Get/Set
  --   <ident> [ { .<ident> } ]
  -- May raise Invalid_Name, Name_Too_Long or Data_Too_Long
  procedure Set (Name : in String; Data : in String);


  -- Declare an alias for and item
  -- Alias must be a valid name as for Get/Set
  -- What has to be either empty or a valid name
  -- Set_Alias ("foo", "bar") declares that
  -- - Set("foo", data) will modify or create the item named "bar"
  --    and notifications to "bar" are sent
  -- - Get("foo") will return the data of "bar" or raises No_Item
  -- - Notification requests on the item named "foo" are accepted
  --    but will not work as long as the alias is valid
  --    (because this item cannot be Set, nor Get)
  -- - Same applies to operations on "foo.stuff" performed realy
  --    on "bar.stuff"
  -- Alias can be canceled with an empty What string.
  -- May raise Invalid_Name or Name_Too_Long
  procedure Set_Alias (Alias : in String; What : in String);


  -- Get the value of an alias
  -- Alias must be a valid name as for Get/Set
  -- If Alias ("foo", "bar") has been called then
  --  Get_Alias ("foo") return "bar" otherwise it returns "".
  -- May raise Invalid_Name or Name_Too_Long
  function Get_Alias (Alias : in String) return String;


  -- (Un)Notify on an item or an alias
  -- Syntax of name for (Un)Notify is
  --   either  <anything>
  --   or  <ident_or_anyid> [ { . <ident_or_anyid> } ] [ . <anything> ]
  --   anyid is "*" and matches any identifier
  --   anything is "**" and matches any sequence of identifiers (even empty)
  -- No pattern matching between notify and un-notify names,
  --   they have to be the same.
  -- May raise Invalid_Name or Name_Too_Long
  procedure Notify (Name : in String; Item : in Boolean; On : in Boolean);


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

