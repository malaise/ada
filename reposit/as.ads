-- Local implementation of Ada.Strings
with Aski;
package As is
  -- Common definitions for bounded and unbounded strings
  Space : constant Character := Aski.Spc_C;
  -- Exception when Low, High, Index... is out of bounds
  Index_Error : exception;
end As;

