with Sys_Calls, Argument, Normal;
with Errors, Debug;
package body Args is

  Mode_Set : Boolean := False;
  Mode : Channel_Mode_List;

  subtype Prio_Range is Positive range 1 .. 255;
  Prio : Prio_Str;

  procedure Usage is
  begin
    Debug.Put_Error ("Usage: "
                   & Argument.Get_Program_Name
                   & " -n<channel/bus name> -p<service_port> -P<prio> <dest_spec>");
    Debug.Put_Error ("  <dest_spec> ::= -b<bus_lan> | -c<channel_file>");
    raise Errors.Exit_Error;
  end Usage;

  procedure Init is
  begin
    Mode := Get_Mode;
    declare
      S1 : constant String := Get_Name;
      S2 : constant String := Get_Dest;
      S3 : constant String := Get_Client_Port;
      P  : constant Prio_Range
         := Prio_Range'Value (Argument.Get_Parameter(1, "P"));
    begin
      Prio := Normal (P, 3, Gap => '0');
      return;
    end;
  exception
    when Argument.Argument_not_Found =>
      Debug.Put_Error ("ERROR. Argument not found");
      Usage;
    when others =>
      Debug.Put_Error ("ERROR. Invalid argument");
      Usage;
  end Init;

  function Has_Key (Key : in String) return Boolean is
  begin
    declare
      S :  constant String := Argument.Get_Parameter(1, key);
    begin
      return True;
    end;
  exception
    when Argument.Argument_not_Found =>
      return False;
  end Has_Key;
    
    
  function Get_Mode return Channel_Mode_List is
  begin
    if Mode_Set then
      return Mode;
    end if;
    if Has_Key ("b") then
      if Has_Key ("c") then
        -- Both b and c
        Usage;
        return Bus;
      else
        Mode := Bus;
      end if;
    else
      -- Has to be channel or nothing
      if Has_Key ("c") then
        Mode := Channel;
      else
        Usage;
        return Bus;
      end if;
    end if;
    Mode_Set := True;
    return Mode;
  end Get_Mode;

  function Get_Dest return String is
  begin
    if Get_Mode = Bus then
      return Argument.Get_Parameter(1, "b");
    else
      return Argument.Get_Parameter(1, "c");
    end if;
  end Get_Dest;


  function Get_Name return String is
  begin
    return Argument.Get_Parameter(1, "n");
  end Get_Name;


  function Get_Client_Port return String is
  begin
    return Argument.Get_Parameter(1, "p");
  end Get_Client_Port;


  function Get_Prio return Prio_Str is
  begin
    return Prio;
  end Get_Prio;

end Args;

