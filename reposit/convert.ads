-- Host to and from Network conversion
with C_Types;
package Convert is

  -- Convert a Int from network to host
  function Ntoh (Int : C_Types.Uint32) return C_Types.Uint32;

  -- Convert a Int from host to network
  function Hton (Int : C_Types.Uint32) return C_Types.Uint32;

private
  pragma Import (C, Ntoh, "ntohl");
  pragma Import (C, Hton, "htonl");
end Convert;

