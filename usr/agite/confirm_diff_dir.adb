with As.U, Directory;
with Confirm;
function Confirm_Diff_Dir (Path, Name : in String) return Boolean is
  Txt : As.U.Asu_Us := As.U.Tus (Directory.Build_File_Name (Path, Name, ""));
begin
  -- Remove trailing '.' if any
  if not Txt.Is_Null and then Txt.Element (Txt.Length) = '.' then
    Txt.Trail (1);
  end if;
  -- Append a '/' if necessary (i.e. Name is a dir)
  if Txt.Is_Null or else Txt.Element (Txt.Length) /= '/' then
    Txt.Append ('/');
  end if;
  return Confirm ("Diff on directory " & Txt.Image,
                  "This operation may launch many differators");
end Confirm_Diff_Dir;

