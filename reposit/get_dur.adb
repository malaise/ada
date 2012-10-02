with Ada.Text_Io;
function Get_Dur (Str : String) return Duration is
  package Dur_Io is new Ada.Text_Io.Fixed_Io (Duration);

  D : Duration;
  L : Positive;
  Str_Len : Natural;
begin
  -- Locate last significant character of Str
  Str_Len := 0;
  for J in reverse Str'Range loop
    if Str_Len = 0 and then Str(J) /= ' ' then
      Str_Len := J + 1 - Str'First;
    end if;
  end loop;
  if Str_Len = 0 then
    raise Constraint_Error;
  end if;

  Dur_Io.Get (Str, D, L);

  if L /= Str'Last then
    raise Constraint_Error;
  end if;
  return D;

exception
  when others =>
    raise Constraint_Error;
end Get_Dur;

