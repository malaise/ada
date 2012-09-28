with Int_Io;
function Get_Int (Str : String) return Integer is
  I : Integer;
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

  Int_Io.Get (Str, I, L);

  if L /= Str'Last then
    raise Constraint_Error;
  end if;
  return I;

exception
  when others =>
    raise Constraint_Error;
end Get_Int;


