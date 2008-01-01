with Ada.Strings.Unbounded;
with Directory, Argument, Basic_Proc;
with Lister, Output;
package body Targets is


  procedure List (Dots : in Entities.Dots_Kind_List;
                  Only_Dirs : in Boolean;
                  Date1, Date2 : in Entities.Date_Spec_Rec;
                  Recursive : in Boolean;
                  Merge : in Boolean;
                  Args : in Argument_Parser.Parsed_Dscr) is
    Entries : Entities.Entity_List;
    Need_New_Line : Boolean;
    use type Directory.File_Kind_List;

    procedure Do_Dir (Dir : in String; Put_Name : Boolean) is
      Done : Boolean;
      Subdirs : Lister.Dir_List;
      Subdir : Ada.Strings.Unbounded.Unbounded_String;
    begin
      -- Do this dir
      if Need_New_Line then
        -- Insert a New_Line between previous output (files or dir) and current
        Output.New_Line;
      end if;
      Lister.List (Entries, Dir, Dots, Only_Dirs, Date1, Date2);
      if not Merge then
        if Put_Name then
          Output.Put_Dir (Dir);
        end if;
        Output.Put (Entries);
        Output.New_Line;
        Entries.Delete_List;
        Need_New_Line := True;
      end if;
      -- Done except if recursive
      if not Recursive then
        return;
      end if;
      -- Recursive: list subdirs and recurse on each
      Lister.List_Dirs (Dir, Subdirs);
      if Subdirs.Is_Empty then
        return;
      end if;
      Subdirs.Rewind;
      loop
        Subdirs.Read (Subdir, Done => Done);
        -- Recursive invocation
        Do_Dir (Directory.Build_File_Name (
             Dir, Ada.Strings.Unbounded.To_String (Subdir), ""),
                True);
        exit when not Done;
      end loop;
      Subdirs.Delete_List (Deallocate => False);
    end Do_Dir;

  begin
    -- Process files (not dirs) among arguments
    Need_New_Line := False;
    if not Only_Dirs
    and then Args.Get_First_Pos_After_Keys /= 0 then
      for I in Args.Get_First_Pos_After_Keys .. Argument.Get_Nbre_Arg loop
        declare
          File : constant String := Argument.Get_Parameter (Occurence => I);
        begin
          if Directory.File_Kind (File) /= Directory.Dir then
            -- Add this "file"
            Lister.List (Entries, File, Date1, Date2);
          end if;
        exception
          when Directory.Name_Error | Directory.Access_Error =>
            -- Error will be reported when scanning dirs
            null;
        end;
      end loop;
      -- Put and clean result if not merge
      if not Merge then
        Output.Put (Entries);
        if not Entries.Is_Empty then
          Output.New_Line;
        end if;
        Entries.Delete_List;
        Need_New_Line := True;
      end if;
    end if;

    -- If no arg at all, then process "."
    if Args.Get_First_Pos_After_Keys = 0 then
      Do_Dir (".", False);
    end if;

    -- Process dirs, if no arg at all, then process "."
    if Args.Get_First_Pos_After_Keys /= 0 then
      for I in Args.Get_First_Pos_After_Keys .. Argument.Get_Nbre_Arg loop
        declare
          Dir : constant String := Argument.Get_Parameter (Occurence => I);
        begin
          if Directory.File_Kind (Dir) = Directory.Dir then
            -- Add this "Dir"
            Do_Dir (Dir, True);
          end if;
        exception
          when Directory.Name_Error =>
            Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": No such file or directory.");

          when Directory.Access_Error =>
            Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": Permission denied.");

        end;
      end loop;
    end if;

    -- Put complete result if merge
    if Merge then
      Output.Put (Entries);
      Entries.Delete_List;
    end if;
  end List;

end Targets;

