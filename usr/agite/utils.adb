with Ada.Calendar;
with Afpx, Str_Util, Many_Strings, Proc_Family, Chronos, Date_Text;
package body Utils is

  -- If Str fits Width then return Str, padded by spaces if not Align_Left
  -- else return ">>" & tail to match Width (if Tail)
  --   or return head to match Width and "<<" (if not Tail)
  function Normalize (Str : String;
                      Width : Positive;
                      Keep_Tail : Boolean := True;
                      Align_Left : Boolean := True) return String is
    Result : String (1 .. Width) := (others => ' ');
  begin
    if Str'Length <= Width then
      if Align_Left then
        return Str;
      else
        Result (Width - Str'Length + 1 .. Width) := Str;
        return Result;
      end if;
    end if;
    if Keep_Tail then
      return ">>" & Str (Str'Last - Width + 3 .. Str'Last);
    else
      return Str (Str'First .. Str'First + Width - 3) & "<<";
    end if;
  end Normalize;

  -- Remove trailing spaces and Htabs
  function Last_Index (Str : String) return Natural is
  begin
    for I in reverse Str'Range loop
      if not Str_Util.Is_Separator (Str(I)) then
        -- Significant char
        return I;
      end if;
    end loop;
    return 0;
  end Last_Index;

  -- Remove trailing spaces and Htabs
  function Parse_Spaces (Str : String) return String is
    (Str (Str'First .. Last_Index (Str)));

  -- Sigchild callback
  -- This will trigger Afpx.PtG to return Signal_Event
  procedure My_Cb (Unused_Death_Report : in Proc_Family.Death_Rec) is
  begin
    null;
  end My_Cb;

  -- Start a command in background
  procedure Launch (Cmd : in String; Set_Callback : in Boolean := False) is
    Cmd_Str : Many_Strings.Many_String;
    Dummy_Res : Proc_Family.Spawn_Result_Rec;
  begin
    Cmd_Str.Set ("/bin/sh");
    Cmd_Str.Cat ("-c");
    Cmd_Str.Cat (Cmd);
    if Set_Callback then
      Dummy_Res := Proc_Family.Spawn (Cmd_Str, Death_Report => My_Cb'Access);
    else
      Dummy_Res := Proc_Family.Spawn (Cmd_Str);
    end if;
  end Launch;

  -- Start a command in foreground
  procedure Execute (Cmd : in String;
                     Out_Flow : in Command.Flow_Access;
                     Err_Flow : in Command.Flow_Access;
                     Exit_Code : out Command.Exit_Code_Range) is
    Cmd_Str : Many_Strings.Many_String;
  begin
    Cmd_Str.Set (Cmd);
    if not Afpx.Is_Suspended then
      Afpx.Suspend;
      Command.Execute (Cmd_Str, True, Out_Flow, Err_Flow, Exit_Code);
      Afpx.Resume;
    else
      Command.Execute (Cmd_Str, True, Out_Flow, Err_Flow, Exit_Code);
    end if;
  exception
    when others =>
      if Afpx.Is_Suspended then
        Afpx.Resume;
      end if;
      raise;
  end Execute;

  -- Separator to split output text
  -- Space and any non-printable character (Htab, Lf...)
  function Separator (C : Character) return Boolean is
    (C <= ' ' or else C > '~');

  -- Protect text for shell: replace ''' by '\'' and enclose within quotes
  function Protect_Text (Str : in String) return String is
    (if Str = "" then Str
     else "'" & Str_Util.Substit (Str, "'", "'\''", True) & "'");

  -- Get current date
  function Get_Current_Date return Git_If.Iso_Date is
  begin
    return Date_Text.Put (Date_Text.Split (Ada.Calendar.Clock),
                          "%Y-%m-%d %H:%M:%S");
  end Get_Current_Date;

  package body Chrono is
    -- The Chrono
    The_Chrono : Chronos.Chrono_Type;
    -- Duration of the operation
    Delta_Oper : Chronos.Time_Rec;

    -- Reset and start a chrono (before calling a potentially long operation)
    procedure Start is
    begin
      -- Ensure that the chrono is stopped. Reset and restart it.
      The_Chrono.Stop;
      The_Chrono.Reset;
      The_Chrono.Start;
    end Start;

    -- Get intermediate time (end of potentially long operation)
    procedure Ended is
    begin
      -- Stop the chrono and save duration of operation
      The_Chrono.Stop;
      Delta_Oper := The_Chrono.Read;
      -- Reset and restart the chrono
      The_Chrono.Reset;
      The_Chrono.Start;
    end Ended;

    -- Is current time not far enough after intermediate time (so calling
    --  again the potentially long operation should be avoided)
    function Overload return Boolean is
      use type Chronos.Time_Rec;
    begin
      return The_Chrono.Read < Delta_Oper;
    end Overload;
  end Chrono;

end Utils;

