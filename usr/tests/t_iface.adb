-- Take a LAN or host, and optional netmask (default FFFFFFFF meaning host)
-- Find it in network interfaces and display host Id and bcast address
with Basic_Proc, Argument, Socket, Ip_Addr;
procedure T_Iface is

  Host_Lan, Mask : Socket.Host_Id;
  Bcast, Host_For : Socket.Host_Id;
begin

  -- Check arguments
  if Argument.Get_Nbre_Arg = 0
  or else Argument.Get_Nbre_Arg > 2
  or else (Argument.Get_Nbre_Arg = 1
           and then (Argument.Get_Parameter = "-h"
                     or else Argument.Get_Parameter = "--help")) then
    Basic_Proc.Put_Line_Error ("Usage: " & Argument.Get_Program_Name
        & " <host_or_LAN_IP_address> [ <LAN_netmask> ]");
    Basic_Proc.Set_Error_Exit_Code;
    return;
  end if;

  -- Decode arguments
  begin
    Host_Lan := Ip_Addr.Parse (Argument.Get_Parameter (Occurence => 1)).Id;
    if Argument.Get_Nbre_Arg = 2 then
      Mask := Ip_Addr.Parse (Argument.Get_Parameter (Occurence => 2)).Id;
    else
      Mask := Socket.Any_Host;
    end if;
  exception
    when Constraint_Error =>
      Basic_Proc.Put_Line_Error ("Invalid address or mask");
      Basic_Proc.Set_Error_Exit_Code;
      return;
  end;

  -- Get host info and display, then bcast and display
  Host_For := Socket.Local_Host_Id_For (Host_Lan, Mask);
  Basic_Proc.Put_Line_Output ("Host: "
      & Ip_Addr.Image (Host_For));
  Bcast := Socket.Local_Bcast_Of (Host_For);
  Basic_Proc.Put_Line_Output ("Bcast: "
      & Ip_Addr.Image (Bcast));

end T_Iface;

