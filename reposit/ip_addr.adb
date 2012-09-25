with As.U, Int_Image, Str_Util;
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
    Txt : As.U.Asu_Us;
    Dots : array (1 .. 3) of Natural;
    Ip_Addr : Socket.Ip_Address;
    Result : Tcp_Util.Remote_Host;
  begin
    if Addr = "" then
      raise Parse_Error;
    end if;
    -- Try to parse Host_Id
    begin
      Txt := As.U.Tus (Addr);
      -- 3 and only three dots, not contiguous
      if Str_Util.Locate (Txt.Image, ".", Occurence => 4) /= 0 then
        raise Parse_Error;
      end if;
      Dots(1) := Str_Util.Locate (Txt.Image, ".", Occurence => 1);
      Dots(2) := Str_Util.Locate (Txt.Image, ".", Occurence => 2);
      Dots(3) := Str_Util.Locate (Txt.Image, ".", Occurence => 3);
      if Dots(1) = 1 or else Dots(2) = Dots(1)+1 or else
         Dots(3) = Dots(2)+1 or else Dots(3) = Txt.Length
      or else Dots(3) = 0 then
        raise Parse_Error;
      end if;

      -- 4 bytes
      Ip_Addr.A := Socket.Byte(To_Natural(
            Txt.Slice (1, Dots(1)-1)));
      Ip_Addr.B := Socket.Byte(To_Natural(
            Txt.Slice (Dots(1)+1, Dots(2)-1)));
      Ip_Addr.C := Socket.Byte(To_Natural(
            Txt.Slice (Dots(2)+1, Dots(3)-1)));
      Ip_Addr.D := Socket.Byte(To_Natural(
            Txt.Slice (Dots(3)+1, Txt.Length)));
      return (Kind => Tcp_Util.Host_Id_Spec,
              Id   => Socket.Addr2Id (Ip_Addr) );
    exception
      when others =>
        -- Not a "vvv.xxx.yyy.zzz" format
        null;
    end;

    -- Try to parse Host_Name
    Result.Name := As.U.Tus (Addr);
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
    Result.Name := As.U.Tus (Port);
    return Result;

  exception
    when others =>
      raise Parse_Error;
  end Parse;

  -- Same with Tcp_Util.Local_port
  function Parse (Port : String) return Tcp_Util.Local_Port is
    Remote_Port : constant Tcp_Util.Remote_Port := Parse (Port);
  begin
    case Remote_Port.Kind is
      when Tcp_Util.Port_Num_Spec =>
        return (Tcp_Util.Port_Num_Spec, Remote_Port.Num);
      when Tcp_Util.Port_Name_Spec =>
        return (Tcp_Util.Port_Name_Spec, Remote_Port.Name);
    end case;
  end Parse;

  -- Image of a port
  function Port_Image is new Int_Image (Socket.Port_Num);
  function Image (Port : Socket.Port_Num) return String renames Port_Image;

  -- Parse a string at format <addr>:<port> where <addr> and <port>
  --  are processed as in both Parse functions above
  -- <addr>: is supported (and lead to empty port name)
  -- :<port> is supported (and lead to empty host name)
  procedure Parse (Addr_Port : in String;
                   Host : out Tcp_Util.Remote_Host;
                   Port : out Tcp_Util.Remote_Port) is
    Colon : Natural;
  begin
    Colon := Str_Util.Locate (Addr_Port, ":");
    if Colon = 0 or else Addr_Port'Length = 1 then
      -- Only ":" or no ':'
      raise Parse_Error;
    end if;
    if Colon = 1 then
      -- :<port>
      Host := (Tcp_Util.Host_Name_Spec, As.U.Asu_Null);
    else
      Host := Parse (Addr_Port (Addr_Port'First .. Colon - 1));
    end if;
    if Colon = Addr_Port'Last then
      -- <addr>:
      Port := (Tcp_Util.Port_Name_Spec, As.U.Asu_Null);
    else
      Port := Parse (Addr_Port (Colon + 1 .. Addr_Port'Last));
    end if;
  end Parse;

end Ip_Addr;

