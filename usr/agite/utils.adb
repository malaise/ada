with Ada.Characters.Latin_1;
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

  function Parse_Spaces (Str : String) return String is
  begin
    return Str (Str'First .. Last_Index (Str));
  end Parse_Spaces;

end Utils;

