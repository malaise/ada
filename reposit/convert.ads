-- Host to and from Network conversion
package Convert is

  -- Convert a Int from network to host
  function Ntoh (Int : Integer) return Integer;

  -- Convert a Int from host to network
  function Hton (Int : Integer) return Integer;

private
  pragma Interface (C, Ntoh);
  pragma Interface_Name (Ntoh, "ntohl");

  pragma Interface (C, Hton);
  pragma Interface_Name (Hton, "htonl");

end Convert;

