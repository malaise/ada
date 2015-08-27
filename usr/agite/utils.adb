with Str_Util, Many_Strings, Proc_Family;
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
  begin
    return Str (Str'First .. Last_Index (Str));
  end Parse_Spaces;

  -- Sigchild callback
  -- This will trigger Afpx.PtG to return Signal_Event
  procedure My_Cb (Unused_Death_Report : in Proc_Family.Death_Rec) is
  begin
    null;
  end My_Cb;

  -- Start a command in background
  procedure Launch (Command : in String; Set_Callback : in Boolean := False) is
    Cmd : Many_Strings.Many_String;
    Dummy_Res : Proc_Family.Spawn_Result_Rec;
  begin
    Cmd.Set ("/bin/sh");
    Cmd.Cat ("-c");
    Cmd.Cat (Command);
    if Set_Callback then
      Dummy_Res := Proc_Family.Spawn (Cmd, Death_Report => My_Cb'Access);
    else
      Dummy_Res := Proc_Family.Spawn (Cmd);
    end if;
  end Launch;

  -- Separator to split output text
  -- Space and any non-printable character (Htab, Lf...)
  function Separator (C : Character) return Boolean is
  begin
    return C <= ' ' or else C > '~';
  end Separator;

  -- Protect text for shell: replace ''' by '\'' and enclose within quotes
  function Protect_Text (Str : in String) return String is
  begin
    return "'" & Str_Util.Substit (Str, "'", "'\''", True) & "'";
  end Protect_Text;

end Utils;

