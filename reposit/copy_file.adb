-- Like cp, return True if Ok.
with Bloc_Io, Sys_Calls;
function Copy_File (Src_Name, Dst_Name : String) return Boolean is

  -- Blocs of characters
  package Char_Io is new Bloc_Io (Character);
  In_File, Out_File : Char_Io.File_Type;
  Bloc_Size : constant := 1024;
  Chars : Char_Io.Element_Array (1 .. Bloc_Size);

  -- How many blocks
  In_Size : Char_Io.Count;
  In_Blocs_Nb : Char_Io.Count;
  Last_Bloc_Len : Char_Io.Count;

  -- Result of Unlink
  Dummy : Boolean;

  use type Char_Io.Count;
begin
  -- Open files
  Char_Io.Open   (In_File, Char_Io.In_File, Src_Name);
  Char_Io.Create (Out_File, Dst_Name);

  -- Compute nb of blocs and nb of chars in last bloc
  In_Size := Char_Io.Size (In_File);
  if In_Size rem Bloc_Size = 0 then
    In_Blocs_Nb := In_Size / Bloc_Size;
    Last_Bloc_Len := Bloc_Size;
  else
    In_Blocs_Nb := In_Size / Bloc_Size + 1;
    Last_Bloc_Len := In_Size rem Bloc_Size;
  end if;

  -- Copy = loop of read/write
  for I in 1 .. In_Blocs_Nb loop
    -- Bloc len for this iteration
    if I /= In_Blocs_Nb then
      Char_Io.Read  (In_File,  Chars);
      Char_Io.Write (Out_File, Chars);
    else
      Char_Io.Read  (In_File,  Chars(1 .. Last_Bloc_Len));
      Char_Io.Write (Out_File, Chars(1 .. Last_Bloc_Len));
    end if;
  end loop;
  -- Done, close
  Char_Io.Close (In_File);
  Char_Io.Close (Out_File);

  -- Propagate access rights
  Sys_Calls.Set_Rights (Dst_Name, Sys_Calls.File_Stat (Src_Name).Rights);

  return True;
exception
  when others =>
    -- Close and remove dest
    if Char_Io.Is_Open (In_File) then
      Char_Io.Close (In_File);
    end if;
    if Char_Io.Is_Open (Out_File) then
      Char_Io.Close (Out_File);
    end if;
    Dummy := Sys_Calls.Unlink (Dst_Name);
    return False;
end Copy_File;

