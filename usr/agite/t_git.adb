with Ada.Text_Io;
with Argument;
with Utils, Git_If;
procedure T_Git is

  Done : Boolean;

  Vers : Git_If.Version_Rec;
  Root, Path : Utils.Asu_Us;

  Files : Git_If.File_List;
  File_Entry : Git_If.File_Entry_Rec;

  Logs : Git_If.Log_List;
  Log_Entry : Git_If.Log_Entry_Rec;

  Date : Git_If.Iso_Date;
  Comment : Git_If.Comment_5;
  Commits : Git_If.Commit_List;
  Commit_Entry : Git_If.Commit_Entry_Rec;

begin

  Vers := Git_If.Get_Version;
  Ada.Text_Io.Put_Line ("Version is " & Vers.Major'Img & Vers.Medium'Img
                      & Vers.Minor'Img);

  Git_If.Get_Root_And_Path (Root, Path);
  Ada.Text_Io.Put_Line ("Root is >" & Utils.Asu.To_String (Root) & "<");
  Ada.Text_Io.Put_Line ("Path is >" & Utils.Asu.To_String (Path) & "<");

  Git_If.List_Files (Utils.Asu.To_String (Path), Files);
  if Files.Is_Empty then
    Ada.Text_Io.Put_Line ("No file");
  else
    loop
      Files.Read (File_Entry, Done => Done);
      Ada.Text_Io.Put_Line (File_Entry.S2 & File_Entry.S3 & " "
                          & Utils.Asu.To_String (File_Entry.Name)
                          & File_Entry.Kind);
      exit when not Done;
    end loop;
  end if;

  if Argument.Get_Nbre_Arg = 0 then
    Path := Utils.Asu_Tus (".");
  else
    Path := Utils.Asu_Tus (Argument.Get_Parameter (Occurence => 1));
  end if;
  Git_If.List_Log (Utils.Asu_Ts (Path), Logs);
  if Logs.Is_Empty then
    Ada.Text_Io.Put_Line ("No log");
  else
    loop
      Logs.Read (Log_Entry, Done => Done);
      Ada.Text_Io.Put_Line (Log_Entry.Hash & " " & Log_Entry.Date);
      Ada.Text_Io.Put_Line (Utils.Asu_Ts (Log_Entry.Comment(1)));
      Ada.Text_Io.New_Line;
      exit when not Done;
    end loop;
  end if;

  if Argument.Get_Nbre_Arg <= 1 then
    return;
  end if;

  Git_If.List_Commit (Argument.Get_Parameter (Occurence => 2),
                      Date, Comment, Commits);
  Ada.Text_Io.Put_Line (Date);
  for I in Comment'Range loop
    Ada.Text_Io.Put_Line (Utils.Asu_Ts (Comment(I)));
  end loop;
  if Commits.Is_Empty then
     Ada.Text_Io.Put_Line ("No File");
  else
    loop
      Commits.Read (Commit_Entry, Done => Done);
      Ada.Text_Io.Put_Line (Commit_Entry.Status & " "
                          & Utils.Asu_Ts (Commit_Entry.File));
      exit when not Done;
    end loop;
  end if;

end T_Git;

