with Ada.Strings.Unbounded;
with String_Mng.Regex;
-- Normalize a path:
-- - append a "/" if ending by "/." or "/.."
-- - replace any "//" by "/" recusively
-- - then remove any "./",
-- - then recusively replace any "<name>/.." by "" (<name> /= "..")
-- - then recusively replace any leading "/.." by ""
function Normalize_Path (Path : String) return String is
  use Ada.Strings.Unbounded;
  Res : Unbounded_String;
  Start, First, Second : Natural;
  function Asu_Tus (Source : in String) return Unbounded_String
                 renames To_Unbounded_String;
  function Asu_Ts (Source : in Unbounded_String) return String
                 renames To_String;
  Sep_Char : constant Character := '/';
  Sep_Str : constant String := Sep_Char & "";
begin
  -- Append a "/" if ending by "/." or "/.."
  Res := Asu_Tus (Path);
  if (Path'Length >= 2
      and then Path(Path'Last - 1 .. Path'Last) = "/.")
  or else (Path'Length >= 3
           and then Path(Path'Last - 2 .. Path'Last) = "/..") then
    Append (Res, Sep_Char);
  end if;

  -- "//" -> "/" recursively
  Res := Asu_Tus (String_Mng.Regex.Replace (Asu_Ts (Res),
                                            Sep_Char & Sep_Char, Sep_Str,
                                            Nb_Cycles => 0) );

  -- "/./" -> "/"
  Res := Asu_Tus (String_Mng.Regex.Replace (Asu_Ts (Res),
                                            Sep_Char & "\." & Sep_Char, Sep_Str,
                                            Nb_Cycles => 0) );

  -- "<name>/../" -> "" recusively
  -- Start at first significant char
  Start := 1;
  if Element (Res, 1) = Sep_Char then
    Start := Start + 1;
  end if;
  loop
    -- Locate next separator
    First := String_Mng.Locate (Asu_Ts (Res), Sep_Str, Start + 1);
    exit when First = 0;
    -- Locate next separator
    Second := String_Mng.Locate (Asu_Ts (Res), Sep_Str, First + 1);
    exit when Second = 0;
    if Slice (Res, First + 1, Second - 1) = ".."
    and then Slice (Res, Start, First - 1) /= ".." then
      -- Delete "<name>/../", and restart from here
      Delete (Res, Start, Second);
    else
      -- Skip
      Start := First + 1;
    end if;
    exit when Start >= Length (Res);
  end loop;

  -- "^/.." -> ""
  loop
    if String_Mng.Locate (Asu_Ts (Res), Sep_Char & "..") = 1 then
      Delete (Res, Start, 3);
    else
      exit;
    end if;
  end loop;

  -- Done
  return Asu_Ts (Res);

end Normalize_Path;

