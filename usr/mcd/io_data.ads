package Io_Data is

  -- Data sent / read on fifo
  Max_Message_Len : constant := 1024;
  subtype Message_Type is String (1 .. Max_Message_Len);

end Io_Data;

