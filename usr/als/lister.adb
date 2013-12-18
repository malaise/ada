with Ada.Calendar;
with Basic_Proc, Argument, Sys_Calls, Regular_Expressions, Directory,
     Dynamic_List;
with Exit_Code;
package body Lister is

  -- List of file templates
  type Tmpl_Rec is record
    Template : As.U.Asu_Us;
    Regex : Boolean;
  end record;
  package Tmpl_Dyn_List_Mng is new Dynamic_List (Tmpl_Rec);
  package Tmpl_List_Mng renames Tmpl_Dyn_List_Mng.Dyn_List;
  subtype Tmpl_List is Tmpl_List_Mng.List_Type;
  Matches : Tmpl_List;
  Excludes : Tmpl_List;

  -- Total size
  Total_Active : Boolean := False;
  Total_Size : Size_Type := 0;
  Total_Number : Natural := 0;
  procedure Add_Size (Size : in Sys_Calls.Off_T; Count : in Boolean);

  -- Slection criteria
  Only_Dirs, Only_Files, Only_Others : Boolean := False;
  Only_Links : Link_Criteria_List := No_Link;
  Follow_Links : Boolean := False;
  Date1, Date2 : Entities.Date_Spec_Rec;
  Utc : Boolean := False;
  -- Set selection criteria
  procedure Set_Criteria (Only_Dirs, Only_Files : in Boolean;
                          Only_Links : in Link_Criteria_List;
                          Only_Others : in Boolean;
                          Follow_Links : in Boolean;
                          Date1, Date2 : in Entities.Date_Spec_Rec;
                          Utc : in Boolean) is
  begin
    Lister.Only_Dirs := Only_Dirs;
    Lister.Only_Files := Only_Files;
    Lister.Only_Links := Only_Links;
    Lister.Only_Others := Only_Others;
    Lister.Follow_Links := Follow_Links;
    Lister.Date1 := Date1;
    Lister.Date2 := Date2;
    Lister.Utc := Utc;
  end Set_Criteria;

  -- Check that a template or regex is valid, raises Invalid_Template if not
  -- return False if Template is empty
  function Check_Template (Template : in String; Regex : in Boolean)
                          return Boolean is
    Dummy_File : constant String := "Toto";
    Dummy_Result : Boolean;
    pragma Unreferenced (Dummy_Result);
  begin
    if Template = "" then
      return False;
    end if;
    -- Template must have no path
    if Directory.Dirname (Template) /= "" then
      raise Invalid_Template;
    end if;
    if not Regex then
      -- Check template match does not raise exception
      begin
        Dummy_Result := Directory.File_Match (Dummy_File, Template);
      exception
        when Directory.Syntax_Error =>
          raise Invalid_Template;
      end;
    else
      -- Check that regex is Ok
      if not Regular_Expressions.Check (Template) then
        raise Invalid_Template;
      end if;
    end if;
    return True;
  end Check_Template;

  procedure Add_Match (Template : in String; Regex : in Boolean) is
  begin
    if Check_Template (Template, Regex) then
      Matches.Insert ((As.U.Tus (Template), Regex));
    end if;
  end Add_Match;

  procedure Add_Exclude (Template : in String; Regex : in Boolean) is
  begin
    if Check_Template (Template, Regex) then
      Excludes.Insert ( (As.U.Tus (Template), Regex) );
    end if;
  end Add_Exclude;

  -- Does an entiy match a date criteria
  function Match (Date : Ada.Calendar.Time;
                  Crit : Entities.Date_Spec_Rec) return Boolean is
    use type Entities.Date_Oper_List, Ada.Calendar.Time;
  begin
    case Crit.Oper is
      when Entities.None =>
        return True;
      when Entities.Equal =>
        return Date = Crit.Date;
      when Entities.Less_Than =>
        return Date < Crit.Date;
      when Entities.Less_Or_Equal =>
        return Date <= Crit.Date;
      when Entities.Greater_Than =>
        return Date > Crit.Date;
      when Entities.Greater_Or_Equal =>
        return Date >= Crit.Date;
    end case;
  end Match;

  -- Does an entity kind match type criteria
  function Match (Kind : Directory.File_Kind_List;
                  Link_Ok : Boolean) return Boolean is
    use type Directory.File_Kind_List;
  begin
    if not (Only_Dirs or else Only_Files or else Only_Links /= No_Link
            or else Only_Others) then
      -- No criteria => averything matches
      return True;
    end if;
    if Kind = Directory.Dir then
      return Only_Dirs;
    elsif Kind = Directory.File then
      return Only_Files;
    elsif Kind = Directory.Link then
      return Only_Links = All_Links
      or else (Only_Links = Broken_Links and then not Link_Ok);
    else
      -- Any other kind
      return Only_Others;
    end if;
  end Match;

  -- Does a file name match a template or regex
  function Match (File, Template : String; Regex : Boolean) return Boolean is
  begin
    if Regex then
      return Regular_Expressions.Match (Template, File, Strict => True);
    else
      return Directory.File_Match (File, Template);
    end if;
  end Match;

  -- Does an entity match all criteria
  function Match (Ent : Entities.Entity) return Boolean is
    Tmpl : Tmpl_Rec;
    Moved : Boolean;
  begin
    -- Check file type and date
    if not Match (Ent.Kind, Ent.Link_Ok)
    or else not Match (Ent.Modif_Time, Date1)
    or else not Match (Ent.Modif_Time, Date2) then
      return False;
    end if;
    -- Check versus exclusion templates
    if not Excludes.Is_Empty then
      Excludes.Rewind;
      loop
        Excludes.Read (Tmpl, Moved => Moved);
        if Match (Ent.Name.Image, Tmpl.Template.Image, Tmpl.Regex) then
          -- The file matches this exclusion template
          return False;
        end if;
        exit when not Moved;
      end loop;
    end if;
    -- File matches if no matching template
    if Matches.Is_Empty then
      return True;
    end if;
    -- Check versus matching templates
    Matches.Rewind;
    loop
      Matches.Read (Tmpl, Moved => Moved);
      if Match (Ent.Name.Image, Tmpl.Template.Image, Tmpl.Regex) then
        -- The file matches this matching template
        return True;
      end if;
      exit when not Moved;
    end loop;
    -- The file does not match any matching template
    return False;
  end Match;

  -- Read link Name and fill Ent.Link, Ent.Link_Ok, Ent.Link_Kind and
  --  Ent.Link_Rights
  procedure Read_Link (Name : in String; Ent : in out Entities.Entity) is
    Link_Target : As.U.Asu_Us;
    Stat : Sys_Calls.File_Stat_Rec;
  begin
    -- Read link direct target
    begin
      Ent.Link := As.U.Tus (Directory.Read_Link (
          File_Name => Name, Recursive => False));
    exception
      when Directory.Name_Error | Directory.Access_Error =>
        Ent.Link.Set_Null;
        Ent.Link_Ok := False;
        Ent.Link_Kind := Directory.Unknown;
        Ent.Link_Rights := 0;
        return;
    end;
    -- Check if final target exists (and is reachable), and store its kind
    begin
      Link_Target := As.U.Tus (Directory.Read_Link (
          File_Name => Name, Recursive => True));
      Stat := Sys_Calls.File_Stat (Link_Target.Image);
      Ent.Link_Ok := True;
      Ent.Link_Kind := Directory.File_Kind_List (Stat.Kind);
      Ent.Link_Rights := Stat.Rights;
    exception
      when Directory.Name_Error | Directory.Access_Error |
           Directory.Recursive_Link |
           Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
        Ent.Link_Ok := False;
        Ent.Link_Kind := Directory.Unknown;
        Ent.Link_Rights := 0;
        return;
    end;
    -- Store final target and size instead of direct target if Follow_Link
    if Follow_Links then
      if Ent.Link_Ok then
        Ent.Link := Link_Target;
        Ent.Size := Stat.Size;
      else
        Ent.Link.Set_Null;
        Ent.Size := 0;
      end if;
    end if;
  end Read_Link;

  -- Add a file if it matches
  procedure List (Ent_List : in out Entities.Entity_List;
                  File : in String) is
    Ent : Entities.Entity;
    Stat : Sys_Calls.File_Stat_Rec;
    use type Directory.File_Kind_List, Ada.Calendar.Time;
  begin
    -- Prepare list for appending
    Ent_List.Rewind (False, Entities.Entity_List_Mng.Prev);

    -- Read file stat
    begin
      Stat := Sys_Calls.File_Stat (File);
    exception
      when Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
        -- Skip this file
        return;
    end;

    -- Fill entity
    Ent.Name := As.U.Tus (Directory.Basename(File));
    Ent.Kind := Directory.File_Kind_List (Stat.Kind);
    Ent.Modif_Time := Sys_Calls.Time_Of (Stat.Modif_Time);
    if not Utc then
      -- Store modif time in local time
      Ent.Modif_Time := Ent.Modif_Time + Sys_Calls.Gmt_Offset;
    end if;
    Ent.Path := As.U.Tus (Directory.Dirname(File));
    Ent.Rights := Stat.Rights;
    Ent.User_Id := Stat.User_Id;
    Ent.Group_Id := Stat.Group_Id;
    Ent.Size := Stat.Size;
    Ent.Link.Set_Null;
    if Ent.Kind = Directory.Link then
      Read_Link (File, Ent);
    end if;

    -- Check it matches
    if not Match (Ent) then
      return;
    end if;

    -- Insert entity
    Ent_List.Insert (Ent);
    -- Update size
    Add_Size (Ent.Size, True);
  end List;

  -- List content of Dir, possibly dots, matching criteria
  procedure List (Ent_List : in out Entities.Entity_List;
                  Dir : in String;
                  Dots : in Entities.Dots_Kind_List;
                  Count_Dot : in Boolean) is
    Desc : Directory.Dir_Desc;
    Ent : Entities.Entity;
    Stat : Sys_Calls.File_Stat_Rec;
    use type Directory.File_Kind_List, Entities.Dots_Kind_List,
             Ada.Calendar.Time;
  begin
    -- Prepare list for appending
    Ent_List.Rewind (False, Entities.Entity_List_Mng.Prev);
    -- Init Ent with path
    Ent.Path := As.U.Tus (Dir);
    -- Open
    begin
      Desc.Open (Dir);
    exception
      when Directory.Name_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": No such file or directory.");
        Exit_Code.Update (Exit_Code.Error);
        return;
      when Directory.Access_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": Permission denied.");
        Exit_Code.Update (Exit_Code.Error);
        return;
    end;
    -- Add current dir size
    if Total_Active and then Count_Dot then
      Stat := Sys_Calls.File_Stat (Dir);
      Add_Size (Stat.Size, False);
    end if;

    -- For each entry
    loop
      declare
        Discard : exception;
      begin
        -- Read next entry
        begin
          Ent.Name := As.U.Tus (Desc.Next_Entry);
        exception
          when Directory.End_Error =>
            -- Done
            exit;
        end;

        -- Check if file name matches dot criteria
        declare
          Str : constant String := Ent.Name.Image;
        begin
          if (Str = "." or else Str = "..")
          and then Dots /= Entities.Basic_Dots_Roots then
            -- Discard "." and ".." except if Basic_Dots_Roots
            raise Discard;
          end if;
          if Str(1) = '.' and then Dots = Entities.Basic then
            -- Discard .* if Basic
            raise Discard;
          end if;
        end;

        begin
          -- Read file stat
          Stat := Sys_Calls.File_Stat (
             Directory.Build_File_Name (Dir, Ent.Name.Image, ""));
        exception
          when Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
            -- Skip this file
            raise Discard;
        end;

        -- Fill entity
        Ent.Kind := Directory.File_Kind_List (Stat.Kind);
        Ent.Modif_Time := Sys_Calls.Time_Of (Stat.Modif_Time);
        if not Utc then
          -- Store modif time in local time
          Ent.Modif_Time := Ent.Modif_Time + Sys_Calls.Gmt_Offset;
        end if;
        Ent.Rights := Stat.Rights;
        Ent.User_Id := Stat.User_Id;
        Ent.Group_Id := Stat.Group_Id;
        Ent.Size := Stat.Size;
        Ent.Link.Set_Null;
        if Ent.Kind = Directory.Link then
          Read_Link (Directory.Build_File_Name (Dir, Ent.Name.Image, ""),
                     Ent);
        end if;

        -- Check it matches
        if not Match (Ent) then
          raise Discard;
        end if;

        -- Append entity to list
        Ent_List.Insert (Ent);
        -- Update size
        Add_Size (Ent.Size, True);
      exception
        when Discard =>
          -- Discard this file
          null;
      end;

    end loop;

    -- Done
    Desc.Close;
  end List;


  -- Add a dir match or exclude template or regex
  -- Dir will match if no matching template or if it matches one of the
  --  matching templates, and if it does not match any exclude template
  Dir_Match : Tmpl_List;
  Dir_Exclude : Tmpl_List;
  procedure Add_Dir_Match   (Template : in String; Regex : in Boolean) is
  begin
    if Check_Template (Template, Regex) then
      Dir_Match.Insert ((As.U.Tus (Template), Regex));
    end if;
  end Add_Dir_Match;

  procedure Add_Dir_Exclude (Template : in String; Regex : in Boolean) is
  begin
    if Check_Template (Template, Regex) then
      Dir_Exclude.Insert ((As.U.Tus (Template), Regex));
    end if;
  end Add_Dir_Exclude;

  -- Does a dir (full path) match
  function Dir_Matches (Dir : String) return Boolean is
    Tmpl : Tmpl_Rec;
    Moved : Boolean;
  begin
    -- Check versus exclusion templates
    if not Dir_Exclude.Is_Empty then
      Dir_Exclude.Rewind;
      loop
        Dir_Exclude.Read (Tmpl, Moved => Moved);
        if Match (Dir, Tmpl.Template.Image, Tmpl.Regex) then
          -- The file matches this exclusion template
          return False;
        end if;
        exit when not Moved;
      end loop;
    end if;
    -- File matches if no matching template
    if Dir_Match.Is_Empty then
      return True;
    end if;
    -- Check versus matching templates
    Dir_Match.Rewind;
    loop
      Dir_Match.Read (Tmpl, Moved => Moved);
      if Match (Dir, Tmpl.Template.Image, Tmpl.Regex) then
        -- The file matches this matching template
        return True;
      end if;
      exit when not Moved;
    end loop;
    -- The file does not match any matching template
    return False;
  end Dir_Matches;

  -- List subdirs of Dir
  procedure List_Dirs (Dir : in String;
                       List : out Dir_List) is
    Desc : Directory.Dir_Desc;
    Str : As.U.Asu_Us;
    use type Directory.File_Kind_List;
  begin
    -- Prepare list
    List.Delete_List (Deallocate => False);

    -- Open
    begin
      Desc.Open (Dir);
    exception
      when Directory.Name_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": No such file or directory.");
        Exit_Code.Update (Exit_Code.Error);
        return;
      when Directory.Access_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": Permission denied.");
        Exit_Code.Update (Exit_Code.Error);
        return;
    end;

    -- For each entry
    loop
      begin
        Str := As.U.Tus (Desc.Next_Entry);
      exception
        when Directory.End_Error =>
          -- Done
          exit;
      end;

      -- Check if it is a directory and matches
      declare
        Lstr : constant String := Str.Image;
        Fstr : constant String := Directory.Build_File_Name (Dir, Lstr, "");
        Kind : Directory.File_Kind_List;
        Link_Target : As.U.Asu_Us;
      begin
        if Lstr /= "."
        and then Lstr /= ".." then
          Kind := Directory.File_Kind (Fstr);
          if Kind = Directory.Dir and then Dir_Matches (Lstr) then
            -- Directory matches: Append entity to list
            List.Insert (Str);
          elsif Follow_Links and then Kind = Directory.Link
          and then Dir_Matches (Lstr) then
            -- Follow_Link and link matches, check it is a directory
            begin
              Link_Target := As.U.Tus (Directory.Read_Link (
                  File_Name => Fstr, Recursive => True));
              Kind := Directory.File_Kind_List(Sys_Calls.File_Stat
                  (Link_Target.Image).Kind);
            exception
              when others =>
                -- Skip unreadable link
                raise Directory.Access_Error;
            end;
            if Kind = Directory.Dir then
              -- Link points to a dir: Append entity to list
              List.Insert (Link_Target);
            end if;
          end if;
        end if;
      exception
        when Directory.Access_Error =>
          -- Skip
          null;
      end;
    end loop;

    -- Done
    Desc.Close;
  end List_Dirs;

  -- Activate, then later get grand total
  procedure Activate_Total is
  begin
    Total_Active := True;
  end Activate_Total;

  function Get_Total return Size_Type is
  begin
    return Total_Size;
  end Get_Total;

  function Get_Number return Natural is
  begin
    return Total_Number;
  end Get_Number;

  procedure Add_Size (Size : in Sys_Calls.Off_T; Count : in Boolean) is
  begin
    if not Total_Active then
      return;
    end if;
    begin
      Total_Size := Total_Size + Size_Type(Size);
    exception
      when Constraint_Error =>
        Total_Size := Size_Type'Last;
    end;
    if not Count then
      return;
    end if;
    begin
      Total_Number := Total_Number + 1;
    exception
      when Constraint_Error =>
        Total_Number := Natural'Last;
    end;
  end Add_Size;
end Lister;

