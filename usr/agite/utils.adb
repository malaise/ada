with Ada.Characters.Latin_1;
with Many_Strings, Proc_Family;
package body Utils is

  -- If Str fits Width then return Str
  -- else return ">>" & tail to match Width (if Tail)
  --   or return head to match Width and "<<" (if not Tail)
  function Normalize (Str : String;
                      Width : Positive;
                      Tail : Boolean := True) return String is
  begin
    if Str'Length <= Width then
      return Str;
    end if;
    if Tail then
      return ">>" & Str (Str'Last - Width + 3 .. Str'Last);
    else
      return Str (Str'First .. Str'First + Width - 3) & "<<";
    end if;
  end Normalize;

  -- Remove trailing spaces and Htabs
  function Last_Index (Str : String) return Natural is
  begin
    for I in reverse Str'Range loop
      if Str(I) /= ' ' and then Str(I) /= Ada.Characters.Latin_1.Ht then
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
  procedure My_Cb (Death_Report : in Proc_Family.Death_Rec) is
    pragma Unreferenced (Death_Report);
  begin
    null;
  end My_Cb;

  -- Start a command in background
  procedure Launch (Command : in String; Set_Callback : in Boolean := False) is
    Cmd : Many_Strings.Many_String;
    Res : Proc_Family.Spawn_Result_Rec;
    pragma Unreferenced (Res);
  begin
    Cmd.Set ("/bin/sh");
    Cmd.Cat ("-c");
    Cmd.Cat (Command);
    if Set_Callback then
      Res := Proc_Family.Spawn (Cmd, Death_Report => My_Cb'Access);
    else
      Res := Proc_Family.Spawn (Cmd);
    end if;
  end Launch;

end Utils;

