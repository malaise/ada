with As.U, Directory, Argument, Basic_Proc;
with Lister, Output, Exit_Code;
package body Targets is


  function List (Dots : Entities.Dots_Kind_List;
                 Recursive : Boolean;
                 Depth : Natural;
                 Merge : Boolean;
                 Skip_Dirs : Boolean;
                 Put_Dir_Names : Boolean;
                 Args : Argument_Parser.Parsed_Dscr) return Boolean is
    Found : Boolean;
    Entries : Entities.Entity_List;
    Need_New_Line : Boolean;
    use type Directory.File_Kind_List;

    function Do_Dir (Dir : String; Put_Name : Boolean; Level : Positive) return Boolean is
      Found : Boolean;
      Moved : Boolean;
      Subdirs : Lister.Dir_List;
      Subdir : As.U.Asu_Us;
    begin
      Found := False;
      -- Do this dir
      if Need_New_Line then
        -- Insert a New_Line between previous output (files or dir) and current
        Output.New_Line;
      end if;
      Lister.List (Entries, Dir, Dots);
      if not Merge then
        if Put_Name then
          Output.Put_Dir (Dir);
        end if;
        if not Entries.Is_Empty then
          Found := True;
          Output.Put (Entries, True);
          Need_New_Line := True;
          Entries.Delete_List;
        end if;
      end if;
      -- Done except if recursive
      if not Recursive then
        return Found;
      elsif Depth /= 0 and then Level > Depth then
        -- Depth level is reached
        return Found;
      end if;
      -- Recursive: list subdirs and recurse on each
      Lister.List_Dirs (Dir, Subdirs);
      if Subdirs.Is_Empty then
        return Found;
      end if;
      Subdirs.Rewind;
      loop
        Subdirs.Read (Subdir, Moved => Moved);
        -- Recursive invocation
        Found := Found or Do_Dir (Directory.Build_File_Name (
             Dir, Subdir.Image, ""), Put_Dir_Names, Level + 1);
        exit when not Moved;
      end loop;
      Subdirs.Delete_List (Deallocate => False);
      return Found;
    end Do_Dir;

  begin
    Found := False;
    -- Process files (not dirs) among arguments
    Need_New_Line := False;
    if Args.Get_First_Pos_After_Keys /= 0 then
      for I in Args.Get_First_Pos_After_Keys .. Argument.Get_Nbre_Arg loop
        declare
          File : constant String := Argument.Get_Parameter (Occurence => I);
        begin
          if File /= ""
          and then Directory.File_Kind (File) /= Directory.Dir then
            -- Add this "file" if it matches
            Lister.List (Entries, File);
          end if;
        exception
          when Directory.Name_Error | Directory.Access_Error =>
            -- Error will be reported when scanning dirs
            null;
        end;
      end loop;
      -- Put and clean result if not merge
      if not Merge then
        if not Entries.Is_Empty then
          Found := True;
          Output.Put (Entries, True);
          Need_New_Line := True;
          Entries.Delete_List;
        end if;
      end if;
    end if;

    -- If no arg at all, then process "."
    if Args.Get_First_Pos_After_Keys = 0
    or else Argument.Get_Parameter (
          Occurence => Args.Get_First_Pos_After_Keys) = "" then
      Found := Found or Do_Dir (".", False, 1);
    end if;

    -- Process dirs that match
    if Args.Get_First_Pos_After_Keys /= 0
    and then not Skip_Dirs then
      for I in Args.Get_First_Pos_After_Keys .. Argument.Get_Nbre_Arg loop
        declare
          Dir : constant String := Argument.Get_Parameter (Occurence => I);
        begin
          if Dir /= ""
          and then Directory.File_Kind (Dir) = Directory.Dir
          and then Lister.Dir_Matches (Dir) then
            -- Add this "Dir"
            Found := Found or Do_Dir (Dir, Put_Dir_Names, 1);
          end if;
        exception
          when Directory.Name_Error =>
            Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": No such file or directory.");
            Exit_Code.Update (Exit_Code.Error);

          when Directory.Access_Error =>
            Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": Permission denied.");
            Exit_Code.Update (Exit_Code.Error);

        end;
      end loop;
    end if;

    -- Put complete result if merge
    if Merge then
      Output.Put (Entries, False);
      Entries.Delete_List;
    end if;
    return Found;
  end List;

end Targets;

