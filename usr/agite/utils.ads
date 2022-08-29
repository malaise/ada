with Command;
with Git_If;
package Utils is

  -- If Str fits Width then return Str, padded with space if no Align_Left
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
  procedure Launch (Cmd : in String; Set_Callback : in Boolean := False);

  -- Start a command in foreground, set both flows
  procedure Execute (Cmd : in String;
                     Out_Flow : in Command.Flow_Access;
                     Err_Flow : in Command.Flow_Access;
                     Exit_Code : out Command.Exit_Code_Range);

  -- Separator to split output text
  function Separator (C : Character) return Boolean;

  -- Protect text for shell: replace ''' by "'\''" and enclose within '''
  function Protect_Text (Str : in String) return String;

  -- For all modules receiving Break_Key from Afpx (Ctrl-C)
  Exit_Requested : exception;

  -- Get current date
  function Get_Current_Date return Git_If.Iso_Date;

  -- "YYYY-MM-DD HH:MM:SS" -> "YYMMDD-HH:MM "
  function Image (Date : Git_If.Iso_Date) return String;

  package Chrono is
    -- Reset and start a chrono (before calling a potentially long operation)
    procedure Start;
    -- Get intermediate time (end of potentially long operation)
    procedure Ended;
    -- Is current time not far enough after intermediate time (so calling
    --  again the potentially long operation should be avoided)
    function Overload return Boolean;
  end Chrono;

end Utils;

