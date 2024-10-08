with As.U, Images, Str_Util;
package body Ip_Addr is

  function Byte_Image is new Images.Mod_Image (Socket.Byte);

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
  --   return the Socket_Util.Remote_Host (Socket_Util.Host_Id_Spec)
  -- Elsif Addr is empty or made of only numbers and dots, raise Parse_Error
  -- Else
  --   return the Socket_Util.Remote_Host (Socket_Util.Host_Name_Spec)
  -- End if
  function Parse (Addr : String) return Socket_Util.Remote_Host is
    Result : Socket_Util.Remote_Host;
    Txt : As.U.Asu_Us;
    Dots : array (1 .. 3) of Natural;
    Ip_Addr : Socket.Ip_Address;
  begin
    if Addr = "" then
      raise Parse_Error;
    end if;
    -- Default result
    Result.Name := As.U.Tus (Addr);

    -- Check if Addr is an addr (contains only nums and dots): X[.Y[.Z  ... ]]
    -- Give up otherwise
    for I in Addr'Range loop
      if (Addr(I) < '0' or else Addr(I) > '9')
      and then (I = Addr'First or else Addr(I) /= '.') then
        return Result;
      end if;
    end loop;

    -- Try to parse Host_Id
    Txt := As.U.Tus (Addr);
    -- Three and only three dots, not contiguous
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
    return (Kind => Socket_Util.Host_Id_Spec,
            Id   => Socket.Addr2Id (Ip_Addr) );
  exception
    when others =>
      raise Parse_Error;
  end Parse;

  -- Image of an Ip address: "xxx.yyy.zzz.ttt"
  function Image  (Addr : Socket.Ip_Address) return String is
    (Byte_Image (Addr.A) & '.'
   & Byte_Image (Addr.B) & '.'
   & Byte_Image (Addr.C) & '.'
   & Byte_Image (Addr.D));

  -- Image of a host Id
  function Image  (Host : Socket.Host_Id) return String is
    (Image (Socket.Id2Addr (Host)));

  -- If Port is a num between 0 and 65535 then
  --   return the Socket_Util.Remote_Port (Socket_Util.Port_Num_Spec)
  -- Else
  --   return the Socket_Util.Remote_Port (Socket_Util.Port_Name_Spec)
  -- End if
  function Parse (Port : String) return Socket_Util.Remote_Port is
    Result : Socket_Util.Remote_Port;
  begin
    if Port = "" then
      raise Parse_Error;
    end if;
    -- Try to parse Port_Num
    begin
      return (Kind => Socket_Util.Port_Num_Spec,
              Num  => Socket_Util.Port_Num(To_Natural (Port)));
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

  -- Same with Socket_Util.Local_port
  function Parse (Port : String) return Socket_Util.Local_Port is
    Remote_Port : constant Socket_Util.Remote_Port := Parse (Port);
  begin
    case Remote_Port.Kind is
      when Socket_Util.Port_Num_Spec =>
        return (Socket_Util.Port_Num_Spec, Remote_Port.Num);
      when Socket_Util.Port_Name_Spec =>
        return (Socket_Util.Port_Name_Spec, Remote_Port.Name);
    end case;
  end Parse;

  -- Image of a port
  function Port_Image is new Images.Mod_Image (Socket.Port_Num);
  function Image (Port : Socket.Port_Num) return String renames Port_Image;

  -- Parse a string at format <addr>:<port> where <addr> and <port>
  --  are processed as in both Parse functions above
  -- <addr>: is supported (and lead to empty port name)
  -- :<port> is supported (and lead to empty host name)
  procedure Parse (Addr_Port : in String;
                   Host : out Socket_Util.Remote_Host;
                   Port : out Socket_Util.Remote_Port) is
    Colon : Natural;
  begin
    Colon := Str_Util.Locate (Addr_Port, Sep);
    if Colon = 0 or else Addr_Port'Length = 1 then
      -- Only ":" or no ':'
      raise Parse_Error;
    end if;
    if Colon = 1 then
      -- :<port>
      Host := (Socket_Util.Host_Name_Spec, As.U.Asu_Null);
    else
      Host := Parse (Addr_Port (Addr_Port'First .. Colon - 1));
    end if;
    if Colon = Addr_Port'Last then
      -- <addr>:
      Port := (Socket_Util.Port_Name_Spec, As.U.Asu_Null);
    else
      Port := Parse (Addr_Port (Colon + 1 .. Addr_Port'Last));
    end if;
  end Parse;

  -- Parse a string at format [<port>:]<addr>:<port> where <addr> and <port>
  --  are processed as in both Parse functions above
  -- If the first <port>: is not provided, this leads to empty port name
  -- <addr>: is supported (and leads to empty port name)
  -- :<port> is supported (and leads to empty host name)
  procedure Parse (Addr_Port : in String;
                   Port1 : out Socket_Util.Local_Port;
                   Host  : out Socket_Util.Remote_Host;
                   Port2 : out Socket_Util.Remote_Port) is
    Colon : Natural;
  begin
    if Str_Util.Locate (Addr_Port, Sep, Occurence => 2) /= 0 then
      -- <port>:<addr>:<port>
      Colon := Str_Util.Locate (Addr_Port, Sep);
      Port1 := Parse (Addr_Port (Addr_Port'First .. Colon - 1));
      Parse (Addr_Port (Colon + 1 .. Addr_Port'Last), Host, Port2);
    else
      -- <addr>:<port>
      Port1 := (others => <>);
      Parse (Addr_Port, Host, Port2);
    end if;
  end Parse;


  -- Image <addr>:<port> (xxx.yyy.zzz.ttt:pppp) of an Addr and Port
  function Image  (Addr : Socket.Ip_Address;
                   Port : Socket.Port_Num) return String is
    (Image (Addr) & Sep & Image (Port));
  function Image  (Host : Socket.Host_Id;
                   Port : Socket.Port_Num) return String is
    (Image (Host) & Sep & Image (Port));


  -- Resolve a remote Host or LAN
  -- If the Host is already a Host_Id_Spec then simply extract the Host_Id
  --  otherwise use Socket.Host_Id_Of or Socket.Lan_Id_Of, which may raise
  --  Name_Error
  function Resolve (Host : Socket_Util.Remote_Host;
                    Lan  : Boolean) return Socket.Host_Id is
    use type Socket_Util.Remote_Host_List;
  begin
    if Host.Kind = Socket_Util.Host_Id_Spec then
      return Host.Id;
    elsif Lan then
      return Socket.Lan_Id_Of (Host.Name.Image);
    else
      return Socket.Host_Id_Of (Host.Name.Image);
    end if;
  end Resolve;

  -- Resolve a remote Port
  -- If the Port is already a Port_Num_Spec then simply extract the Port_Num
  --  otherwise use Socket.Port_Num_Of, which may raise Name_Error
  function Resolve (Port : Socket_Util.Remote_Port;
                    Protocol : Socket.Protocol_List) return Socket.Port_Num is
    use type Socket_Util.Remote_Port_List;
  begin
    if Port.Kind = Socket_Util.Port_Num_Spec then
      return Port.Num;
    else
      return Socket.Port_Num_Of (Port.Name.Image, Protocol);
    end if;
  end Resolve;

  function Resolve (Port : Socket_Util.Local_Port;
                    Protocol : Socket.Protocol_List) return Socket.Port_Num is
    use type Socket_Util.Local_Port_List;
  begin
    if Port.Kind = Socket_Util.Port_Num_Spec then
      return Port.Num;
    elsif Port.Kind = Socket_Util.Port_Name_Spec then
      return Socket.Port_Num_Of (Port.Name.Image, Protocol);
    else
      raise Constraint_Error;
    end if;
  end Resolve;

end Ip_Addr;

