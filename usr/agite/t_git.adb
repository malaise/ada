with As.U, Argument, Basic_Proc;
with Git_If;
procedure T_Git is

  Moved, Done : Boolean;

  Vers : Git_If.Version_Rec;
  Root, Path : As.U.Asu_Us;

  Files : Git_If.File_List;
  File_Entry : Git_If.File_Entry_Rec;

  Logs : Git_If.Log_List;
  Log_Entry : Git_If.Log_Entry_Rec;

  Hash : Git_If.Git_Hash;
  Merged : Boolean;
  Date : Git_If.Iso_Date;
  Comment : Git_If.Comment_Array (1 ..20);
  Commits : Git_If.Commit_List;
  Commit_Entry : Git_If.Commit_Entry_Rec;

begin

  Vers := Git_If.Get_Version;
  Basic_Proc.Put_Line_Output ("Version is " & Vers.Major'Img & Vers.Medium'Img
                      & Vers.Minor'Img);

  Git_If.Get_Root_And_Path (Root, Path);
  Basic_Proc.Put_Line_Output ("Root is >" & Root.Image & "<");
  Basic_Proc.Put_Line_Output ("Path is >" & Path.Image & "<");

  Git_If.List_Files (Path.Image, Files);
  if Files.Is_Empty then
    Basic_Proc.Put_Line_Output ("No file");
  else
    loop
      Files.Read (File_Entry, Moved => Moved);
      Basic_Proc.Put_Line_Output (File_Entry.S2 & File_Entry.S3 & " "
                          & File_Entry.Name.Image
                          & File_Entry.Kind);
      exit when not Moved;
    end loop;
  end if;

  if Argument.Get_Nbre_Arg = 0 then
    Path := As.U.Tus (".");
  else
    Argument.Get_Parameter (Path, Occurence => 1);
  end if;
  Git_If.List_Log (Path.Image, 0, Logs, Done);
  if Logs.Is_Empty then
    Basic_Proc.Put_Line_Output ("No log");
  else
    loop
      Logs.Read (Log_Entry, Moved => Moved);
      Basic_Proc.Put_Line_Output (Log_Entry.Hash & " " & Log_Entry.Date);
      Basic_Proc.Put_Line_Output (Log_Entry.Comment(1).Image);
      Basic_Proc.New_Line_Output;
      exit when not Moved;
    end loop;
  end if;

  if Argument.Get_Nbre_Arg <= 1 then
    return;
  end if;

  Git_If.List_Commit (Argument.Get_Parameter (Occurence => 2),
                      Hash, Merged, Date, Comment, Commits);
  Basic_Proc.Put_Line_Output (Hash);
  Basic_Proc.Put_Line_Output ("Merged: " & Merged'Img);
  Basic_Proc.Put_Line_Output (Date);
  for I in Comment'Range loop
    Basic_Proc.Put_Line_Output (Comment(I).Image);
  end loop;
  if Commits.Is_Empty then
     Basic_Proc.Put_Line_Output ("No File");
  else
    loop
      Commits.Read (Commit_Entry, Moved => Moved);
      Basic_Proc.Put_Line_Output (Commit_Entry.Status & " "
                          & Commit_Entry.File.Image);
      exit when not Moved;
    end loop;
  end if;

end T_Git;

