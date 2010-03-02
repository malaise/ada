with Ada.Text_Io;
with Git_If;
procedure T_Git is

  Path : Git_If.Asu_Us;
  Files : Git_If.File_List;
  File_Entry : Git_If.File_Entry_Rec;
  Done : Boolean;

begin

  Path := Git_If.Get_Path;
  Ada.Text_Io.Put_Line ("Path is >" & Git_If.Asu.To_String (Path) & "<");

  Git_If.List_Files (Git_If.Asu.To_String (Path), Files);
  if Files.Is_Empty then
    Ada.Text_Io.Put_Line ("No file");
  else
    Files.Rewind;
    loop
      Files.Read (File_Entry, Done => Done);
      Ada.Text_Io.Put_Line (File_Entry.S2 & File_Entry.S3 & " "
                          & Git_If.Asu.To_String (File_Entry.Name)
                          & File_Entry.Kind);
      exit when not Done;
    end loop;
  end if;

end T_Git;

