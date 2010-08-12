with Text_Handler, Int_Image, String_Mng;
package body Ip_Addr is

  function Byte_Image is new Int_Image (Socket.Byte);

  -- Return -1 if convestion impossible or greater than Max
  function To_Natural (Str : String) return Integer is
    Num : Natural;
    Error : constant Integer := -1;
  begin
    begin
      Num := Natural'Value (Str);
    exception
      when others =>
        return Error;
    end;
    return Num;
  end To_Natural;

  -- If Addr is "xxx.yyy.zzz.ttt" where each is between 0 and 255 then
  --   return the Tcp_Util.Remote_Host (Tcp_Util.Host_Id_Spec)
  -- Else
  --   return the Tcp_Util.Remote_Host (Tcp_Util.Host_Name_Spec)
  -- End if
  function Parse (Addr : String) return Tcp_Util.Remote_Host is
    Txt : Text_Handler.Text (Tcp_Util.Max_Host_Name_Len);
    Dots : array (1 .. 3) of Natural;
    Ip_Addr : Socket.Ip_Address;
    Result : Tcp_Util.Remote_Host;
  begin
    if Addr = "" then
      raise Parse_Error;
    end if;
    -- Try to parse Host_Id
    begin
      Text_Handler.Set (Txt, Addr);
      -- 3 and only three dots, not contiguous
      if Text_Handler.Locate (Txt, '.', 4) /= 0 then
        raise Parse_Error;
      end if;
      Dots(1) := Text_Handler.Locate (Txt, '.', 1);
      Dots(2) := Text_Handler.Locate (Txt, '.', 2);
      Dots(3) := Text_Handler.Locate (Txt, '.', 3);
      if Dots(1) = 1 or else Dots(2) = Dots(1)+1 or else
         Dots(3) = Dots(2)+1 or else Dots(3) = Text_Handler.Length(Txt) then
        raise Parse_Error;
      end if;

      -- 4 bytes
      Ip_Addr.A := Socket.Byte(To_Natural(
            Text_Handler.Value(Txt)(1 .. Dots(1)-1)));
      Ip_Addr.B := Socket.Byte(To_Natural(
            Text_Handler.Value(Txt)(Dots(1)+1 .. Dots(2)-1)));
      Ip_Addr.C := Socket.Byte(To_Natural(
            Text_Handler.Value(Txt)(Dots(2)+1 .. Dots(3)-1)));
      Ip_Addr.D := Socket.Byte(To_Natural(
            Text_Handler.Value(Txt)(Dots(3)+1 .. Text_Handler.Length(Txt))));
      return (Kind => Tcp_Util.Host_Id_Spec,
              Id   => Socket.Addr2Id (Ip_Addr) );
    exception
      when others =>
        null;
    end;

    -- Try to parse Host_Name
    Result.Name (1 .. Addr'Length) := Addr;
    return Result;

  exception
    when others =>
      raise Parse_Error;

  end Parse;

  -- Image of an Ip address: "xxx.yyy.zzz.ttt"
  function Image  (Addr : Socket.Ip_Address) return String is
  begin
    return Byte_Image (Addr.A) & '.'
         & Byte_Image (Addr.B) & '.'
         & Byte_Image (Addr.C) & '.'
         & Byte_Image (Addr.D);
  end Image;

  -- If Port is a num between 0 and 65535 then
  --   return the Tcp_Util.Remote_Port (Tcp_Util.Port_Num_Spec)
  -- Else
  --   return the Tcp_Util.Remote_Port (Tcp_Util.Port_Name_Spec)
  -- End if
  function Parse (Port : String) return Tcp_Util.Remote_Port is
    Result : Tcp_Util.Remote_Port;
  begin
    if Port = "" then
      raise Parse_Error;
    end if;
    -- Try to parse Port_Num
    begin
      return (Kind => Tcp_Util.Port_Num_Spec,
              Num  => Tcp_Util.Port_Num(To_Natural (Port)));
    exception
      when others =>
        null;
    end;

    -- Try to parse Port_Name
    Result.Name (1 .. Port'Length) := Port;
    return Result;

  exception
    when others =>
      raise Parse_Error;
  end Parse;

  -- Image of a port
  function Port_Image is new Int_Image (Socket.Port_Num);
  function Image (Port : Socket.Port_Num) return String renames Port_Image;

  -- Parse a string at format <addr>:<port> where <addr> and <port>
  --  are processed as in both Parse functions above
  -- :<port> and <port> are supported (<addr> leads to empty host name)
  -- <addr>: raises Parse_Error
  procedure Parse (Addr_Port : in String;
                   Host : out Tcp_Util.Remote_Host;
                   Port : out Tcp_Util.Remote_Port) is
    Colon : Natural;
  begin
    Colon := String_Mng.Locate (Addr_Port, ":");
    if Colon <= 1 then
      Host := (Tcp_Util.Host_Name_Spec, (others => ' '));
    else
      Host := Parse (Addr_Port (Addr_Port'First .. Colon - 1));
    end if;
    Port := Parse (Addr_Port (Colon + 1 .. Addr_Port'Last));
  end Parse;

end Ip_Addr;

