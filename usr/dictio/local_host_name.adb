with As.U, Socket_Util;
package body Local_Host_Name is

  Lhn : Socket_Util.Host_Name := As.U.Asu_Null;

  procedure Set (Name : in String) is
  begin
    if Name'Length = 0 or else Name'Length > Max_Host_Name_Length then
      raise Constraint_Error;
    end if;
    Lhn := As.U.Tus (Name);
  end Set;

  function Get return String is (Lhn.Image);

  procedure Get (Name : out Host_Name) is
  begin
    Name := (others => ' ');
    Name(1 .. Lhn.Length) := Lhn.Image;
  end Get;

end Local_Host_Name;

