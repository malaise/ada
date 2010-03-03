with Ada.Characters.Latin_1;
with Many_Strings, Proc_Family;
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

  -- Scroll the list
  procedure Scroll (Fld_No : in List_Scroll_Fld_Range) is
  begin
    case Fld_No is
      when 2 => Afpx.Update_List(Afpx.Top);
      when 3 => Afpx.Update_List(Afpx.Page_Up);
      when 4 => Afpx.Update_List(Afpx.Up);
      when 5 => Afpx.Update_List(Afpx.Center);
      when 6 => Afpx.Update_List(Afpx.Down);
      when 7 => Afpx.Update_List(Afpx.Page_Down);
      when 8 => Afpx.Update_List(Afpx.Bottom);
    end case;
  end Scroll;

  -- Start a command in background
  procedure Launch (Command : in String) is
    Cmd : constant String
        := Many_Strings.Cat ("/bin/sh",
             Many_Strings.Cat ("-c", Command));
    Res : Proc_Family.Spawn_Result_Rec;
    pragma Unreferenced (Res);
  begin
    Res := Proc_Family.Spawn (Cmd);
  end Launch;

end Utils;

