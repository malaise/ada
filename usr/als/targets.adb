with As.U.Utils, Directory, Argument, Basic_Proc;
with Lister, Output, Exit_Code, Debug;
package body Targets is

  -- List of absolute paths from origin to current, to detect loops of symlinks
  Rope : As.U.Utils.Asu_Unique_List_Mng.Unique_List_Type;

  function List (Dots : Entities.Dots_Kind_List;
                 Recursive : Boolean;
                 Depth : Natural;
                 Merge : Boolean;
                 Skip_Dirs : Boolean;
                 Put_Dir_Names : Trilean.Trilean;
                 Follow_Links : Boolean;
                 Args : Argument_Parser.Parsed_Dscr) return Boolean is
    Found : Boolean;
    Entries : Entities.Entity_List;
    Need_New_Line : Boolean;
    use type Directory.File_Kind_List, Trilean.Trilean;

    function Do_Dir (Dir : String;
                     Put_Name : Trilean.Trilean;
                     Level : Positive;
                     Count_Dot : Boolean) return Boolean is
      Found : Boolean;
      Moved : Boolean;
      Match : Trilean.Trilean;
      Subdirs : Lister.Dir_List;
      Curdir, Subdir : As.U.Asu_Us;

      -- Remove current dir from rope
      procedure Remove_Current is
      begin
        if Recursive and then Follow_Links then
          Rope.Delete (Curdir);
        end if;
      end Remove_Current;

    begin
      Found := False;
      -- Make full path of current dir ('.' or relative or absolute path)
      Curdir := As.U.Tus (Directory.Make_Full_Path (Dir));
      if Recursive and then Follow_Links then
        if Rope.Search (Curdir) then
          -- Current directory already listed (due to recursive symbolic links)
          Debug.Log ("  Found in rope");
          return Found;
        end if;
        Rope.Insert (Curdir);
      end if;
      Match := Lister.Dir_Matches (Dir);
      if Match = Trilean.True then
        Debug.Log ("  Dir matches");
        -- Do this dir
        Lister.List (Entries, Dir, Dots, Count_Dot);
        if not Merge then
          if Entries.Is_Empty then
            if Put_Name = Trilean.True then
              -- Always put dir name, even if no entity to put
              if Need_New_Line then
                -- Insert New_Line between previous output (files or dir)
                --  and current
                Output.New_Line;
              end if;
              Output.Put_Dir (Dir);
              Need_New_Line := False;
            end if;
          else
            if Need_New_Line then
              -- Insert New_Line between previous output (files or dir)
              --  and current
              Output.New_Line;
            end if;
            if Put_Name /= Trilean.False then
              -- Not Never put dir name
              Output.Put_Dir (Dir);
            end if;
            Found := True;
            Output.Put (Entries, True, True);
            Need_New_Line := Put_Name /= Trilean.False;
            Entries.Delete_List;
          end if;
        end if;
      elsif Match = Trilean.False then
        Debug.Log ("  Dir does not match");
      else
        Debug.Log ("  Dir is discarded");
      end if;
      -- Done if not recursive
      -- Or directory excluded
      -- Or depth reached
      if not Recursive
      or else Match = Lister.Discard
      or else (Depth /= 0 and then Level > Depth) then
        Remove_Current;
        return Found;
      end if;
      -- Recursive: list subdirs and recurse on each
      Lister.List_Dirs (Dir, Subdirs);
      if Subdirs.Is_Empty then
        Remove_Current;
        return Found;
      end if;
      Subdirs.Rewind;
      loop
        Subdirs.Read (Subdir, Moved => Moved);
        if Subdir.Is_Null or else Subdir.Element (1) /= '/' then
          -- Relative path to Dir
          Subdir := As.U.Tus (Directory.Build_File_Name (
              Dir, Subdir.Image, ""));
        end if;
        -- Recursive invocation
        Debug.Log ("Doing subdir " & Subdir.Image);
        Found := Found or --## rule line off Andor_Boolean
                 Do_Dir (Subdir.Image, Put_Dir_Names, Level + 1, False);
        exit when not Moved;
      end loop;
      Subdirs.Delete_List (Deallocate => False);
      Remove_Current;
      return Found;
    end Do_Dir;

  begin
    Found := False;
    Rope.Delete_List;
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
            Debug.Log ("Listing arg " & File);
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
          Output.Put (Entries, False, True);
          Need_New_Line := True;
          Entries.Delete_List;
        end if;
      end if;
    end if;

    -- If no arg at all, then process ""
    if Args.Get_First_Pos_After_Keys = 0
    or else Argument.Get_Parameter (
          Occurence => Args.Get_First_Pos_After_Keys) = "" then
      Debug.Log ("Doing current dir");
      -- No dir name if explicit dir_name=Always
      Found := Found or --## rule line off Andor_Boolean
               Do_Dir ("", (if Put_Dir_Names = Trilean.True then Trilean.True
                             else Trilean.False),
                            1, True);
    end if;

    -- Process dirs that match
    if Args.Get_First_Pos_After_Keys /= 0
    and then not Skip_Dirs then
      for I in Args.Get_First_Pos_After_Keys .. Argument.Get_Nbre_Arg loop
        declare
          Dir : constant String := Argument.Get_Parameter (Occurence => I);
        begin
          if Dir /= ""
          and then Directory.File_Kind (Dir) = Directory.Dir then
            -- Add this "Dir"
            Debug.Log ("Doing dir " & Dir);
            Found := Found or --## rule line off Andor_Boolean
                     Do_Dir (Dir, Put_Dir_Names, 1, True);
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
      Output.Put (Entries, False, False);
      Entries.Delete_List;
    end if;
    return Found;
  end List;

end Targets;

