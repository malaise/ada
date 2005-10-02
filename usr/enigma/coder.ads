-- The Enigma coder
with Types;
package Coder is

  -- Init enigma from arguments and config files
  procedure Init;

  -- Encode a letter
  function Encode (X : Types.Letter) return Types.Letter;

end Coder;

