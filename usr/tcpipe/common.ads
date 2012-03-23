with Socket;
package Common is

  -- Port identifier
  subtype Port_Num is Socket.Port_Num;

  -- Data sent / read on connection
  Max_Data_Len : constant := 1024;
  subtype Data_Type is String (1 .. Max_Data_Len);

end Common;

