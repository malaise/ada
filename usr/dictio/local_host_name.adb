with As.U; use As.U;
with Tcp_Util;
package body Local_Host_Name is

  Lhn : Tcp_Util.Host_Name := Asu_Null;

  procedure Set (Name : in String) is
  begin
    if Name'Length = 0 or else Name'Length > Max_Host_Name_Length then
      raise Constraint_Error;
    end if;
    Lhn := Tus (Name);
  end Set;

  function Get return String is
  begin
    return Lhn.Image;
  end Get;

  procedure Get (Name : out Host_Name) is
  begin
    Name := (others => ' ');
    Name(1 .. Lhn.Length) := Lhn.Image;
  end Get;

end Local_Host_Name;

