with Parse;
package body Local_Host_Name is

  Lhn : Tcp_Util.Host_Name := (others => ' ');

  procedure Set (Name : in String) is
  begin
    Lhn(1 .. Name'Length) := Name;
  end Set;

  function Get return String is
  begin
    return Parse (Lhn);
  end Get;

  procedure Get (Name : out Tcp_Util.Host_Name) is
  begin
    Name := Lhn;
  end Get;

end Local_Host_Name;

