-- with Common;

package body Partner is

  -- Init connection with partner
  -- Try to reconnect infinitely
  -- When receiving a service message handle it
  -- when receiveg a data emssage relay it
  -- Invalid_Addr : exception;

  procedure Init (Client : in Boolean; Addr : in String) is
  begin
    null;
  end Init;

  function Is_Connected return Boolean is
  begin
    return Is_Connected;
  end Is_Connected;

  -- Kind of message between tcpipe instances
  -- type Kind_List is (Data, Connect, Disconnect);

  -- Data sent / read on connection
  -- Max_Data_Len : constant := 1024;

  -- subtype Data_Type is String (1 .. Max_Data_Len);

  -- Message sent between tcpipe instances
  -- type Header is record
  --  Kind : Kind_List;
  --  Port : Common.Port_Num;
  -- end record;

  -- type Message is record
  --  Head : Header;
  --  Data : Data_Type;
  -- end record;

  -- Send connection/disconnection/data
  -- If sending error then close local connect on port
  procedure Send (Msg : in Message) is
  begin
    null;
  end Send;

end Partner;

