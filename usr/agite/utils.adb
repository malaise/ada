with Ada.Characters.Latin_1;
with Many_Strings, Proc_Family, Con_Io;
package body Utils is

  -- If Str fits Width then return Str
  -- else return ">> " & tail
  function Normalize (Str : String; Width : Positive) return String is
  begin
    if Str'Length <= Width then
      return Str;
    end if;
    return ">>" & Str (Str'Last - Width + 3 .. Str'Last);
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

  -- Protect a field and "revert" its colors
  procedure Protect_Field (Field_No : in Afpx.Absolute_Field_Range) is
  begin
    Afpx.Set_Field_Protection (Field_No, True);
    Afpx.Set_Field_Colors (Field_No,
          Foreground => Con_Io.Color_Of ("Black"),
          Background => Afpx.Get_Descriptor_Background);
  end Protect_Field;

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

