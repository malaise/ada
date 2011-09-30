package Utils is

  -- If Str fits Width then return Str
  -- else return ">>" & tail to match Width (if Tail)
  --   or return head to match Width and "<<" (if not Tail)
  function Normalize (Str : String;
                      Width : Positive;
                      Tail : Boolean := True) return String;

  -- Remove trailing spaces and Htabs
  function Parse_Spaces (Str : String) return String;
  function Last_Index (Str : String) return Natural;

  -- Start a command in background
  procedure Launch (Command : in String);

  -- Exception on Ctrl C
  Exit_Requested : exception;

end Utils;

