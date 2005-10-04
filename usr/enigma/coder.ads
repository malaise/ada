-- The Enigma coder
with Types;
package Coder is

  -- Init enigma from arguments and config files
  procedure Init;
  Init_Failed : exception;

  -- Encode a letter
  function Encode (L : Types.Letter) return Types.Letter;

end Coder;

