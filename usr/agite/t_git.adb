with Ada.Text_Io;
with Git_If;
procedure T_Git is

  Vers : Git_If.Version_Rec;
  Root, Path : Git_If.Asu_Us;
  Files : Git_If.File_List;
  File_Entry : Git_If.File_Entry_Rec;
  Done : Boolean;

begin

  Vers := Git_If.Get_Version;
  Ada.Text_Io.Put_Line ("Version is " & Vers.Major'Img & Vers.Medium'Img
                      & Vers.Minor'Img);

  Git_If.Get_Root_And_Path (Root, Path);
  Ada.Text_Io.Put_Line ("Root is >" & Git_If.Asu.To_String (Root) & "<");
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

