with Afpx;
package Utils is

  -- If Str fits Width then return Str
  -- else return ">> " & tail to match Width
  function Normalize (Str : String; Width : Positive) return String;

  -- Remove trailing spaces and Htabs
  function Parse_Spaces (Str : String) return String;
  function Last_Index (Str : String) return Natural;

  -- The scroll buttons
  subtype List_Scroll_Fld_Range is Afpx.Field_Range range 2 .. 8;

  -- Start a command in background
  procedure Launch (Command : in String);

  -- Exception on Ctrl C
  Exit_Requested : exception;

end Utils;

