package Utils is

  -- If Str fits Width then return Str
  -- else return ">>" & tail to match Width (if Keep_Tail)
  --   or return head to match Width and "<<" (if not Keep_Tail)
  function Normalize (Str : String;
                      Width : Positive;
                      Keep_Tail : Boolean := True;
                      Align_Left : Boolean := True) return String;

  -- Remove trailing spaces and Htabs
  function Parse_Spaces (Str : String) return String;
  function Last_Index (Str : String) return Natural;

  -- Start a command in background
  type Client_Callback is access procedure;
  procedure Launch (Command : in String; Set_Callback : in Boolean := False);

  -- Exception on Ctrl C
  Exit_Requested : exception;

end Utils;

