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

  -- Start a command in background
  procedure Launch (Command : in String) is
    Cmd : Many_Strings.Many_String;
    Res : Proc_Family.Spawn_Result_Rec;
    pragma Unreferenced (Res);
  begin
    Cmd.Set ("/bin/sh");
    Cmd.Cat ("-c");
    Cmd.Cat (Command);
    Res := Proc_Family.Spawn (Cmd);
  end Launch;

end Utils;

