with Tcp_Util;
package Local_Host_Name is

  procedure Set (Name : in String);

  function Get return String;

  procedure Get (Name : out Tcp_Util.Host_Name);

end Local_Host_Name;

