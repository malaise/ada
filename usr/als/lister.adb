with Ada.Calendar;
with Basic_Proc, Argument, Sys_Calls.File_Access, Reg_Exp, Directory,
     Dynamic_List;
with Exit_Code, Debug;
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
  Only_Dirs, Only_Files, Only_Others, Only_Rights : Boolean := False;
  Rights : Access_Rights;
  Only_Links : Link_Criteria_List := No_Link;
  Follow_Links : Boolean := False;
  Show_Targets : Boolean := False;
  Date1, Date2 : Entities.Date_Spec_Rec;
  Utc : Boolean := False;
  -- Set selection criteria
  procedure Set_Criteria (Only_Dirs, Only_Files : in Boolean;
                          Rights : in Access_Rights;
                          Only_Links : in Link_Criteria_List;
                          Only_Others : in Boolean;
                          Follow_Links : in Boolean;
                          Show_Targets : in Boolean;
                          Date1, Date2 : in Entities.Date_Spec_Rec;
                          Utc : in Boolean) is
    use all type Trilean.Trilean;
  begin
    Lister.Only_Dirs := Only_Dirs;
    Lister.Only_Files := Only_Files;
    Lister.Rights := Rights;
    Only_Rights := Rights(Read) /= Other or else Rights(Write) /= Other
           or else Rights(Exec) /= Other;
    Lister.Only_Links := Only_Links;
    Lister.Only_Others := Only_Others;
    Lister.Follow_Links := Follow_Links;
    Lister.Show_Targets := Show_Targets;
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
      if not Reg_Exp.Check (Template) then
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
    use type Ada.Calendar.Time;
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

  -- Does and entity access rights match read, write and exec criteria
  function Match (File_User, File_Group : Natural;
                  File_Rights : Natural) return Boolean is
    Can_Read, Can_Write, Can_Exec : Boolean;
    function Match (Crit : Trilean.Trilean; Can : Boolean) return Boolean is
      use all type Trilean.Trilean;
    begin
      -- Always True if Crit is Other, otherwise True if Can = Crit;
      return (if Crit = Other then True else Can = Tri2Boo (Crit));
    end Match;

  begin
    if not Only_Rights then
      -- No criteria on access rights
      return True;
    end if;
    -- Get access rights of current user on this entry
    Sys_Calls.File_Access.Has_Access (File_User, File_Group, File_Rights,
                                      Can_Read, Can_Write, Can_Exec);
    return   Match (Rights(Read),  Can_Read)
    and then Match (Rights(Write), Can_Write)
    and then Match (Rights(Exec),  Can_Exec);
  end Match;

  -- Does a file/dir name (without path) match a template or regex
  function Match (File, Template : String; Regex : Boolean) return Boolean is
     -- Basename v.s. regex
    (if Regex then Reg_Exp.Match (Template, Directory.Basename (File),
                                  Strict => True)
     -- Basename v.s. template
     else Directory.File_Match (Directory.Basename (File), Template));

  -- Does an entity match all criteria
  function Match (Ent : Entities.Entity) return Boolean is
    Tmpl : Tmpl_Rec;
    Moved : Boolean;
  begin
    Debug.Log ("  Checking match for " & Ent.Name.Image);
    -- Check file type and date
    if not Match (Ent.Kind, Ent.Link_Ok)
    or else not Match (Ent.User_Id, Ent.Group_Id, Ent.Rights)
    or else not Match (Ent.Modif_Time, Date1)
    or else not Match (Ent.Modif_Time, Date2) then
      Debug.Log ("    Kind or rights or date mismatch");
      return False;
    end if;
    -- Check versus exclusion templates
    if not Excludes.Is_Empty then
      Excludes.Rewind;
      loop
        Excludes.Read (Tmpl, Moved => Moved);
        if Match (Ent.Name.Image, Tmpl.Template.Image, Tmpl.Regex) then
          -- The file matches this exclusion template
          Debug.Log ("    Match exclusion " & Tmpl.Template.Image);
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
        Debug.Log ("    Match " & Tmpl.Template.Image);
        return True;
      end if;
      exit when not Moved;
    end loop;
    -- The file does not match any matching template
    Debug.Log ("    No match");
    return False;
  end Match;

  -- Read link Name and fill Ent.Link, Ent.Link_Ok, Ent.Link_Kind and
  --  Ent.Link_Rights
  procedure Read_Link (Name : in String; Ent : in out Entities.Entity) is
    Link_Target : As.U.Asu_Us;
    Stat : Sys_Calls.File_Stat_Rec;
    Result : Directory.Link_Result;
    use type Directory.Link_Result;
  begin
    -- Default result (on error)
    Ent.Link.Set_Null;
    Ent.Link_Ok := False;
    Ent.Link_Kind := Directory.Unknown;
    Ent.Link_Rights := 0;
    -- Read link direct target
    begin
      Ent.Link := As.U.Tus (Directory.Read_Link (
          File_Name => Name, Recursive => False));
    exception
      when Directory.Name_Error | Directory.Access_Error =>
        return;
    end;
    -- Check if final target exists (and is reachable)
    Result := Directory.Scan_Link (Name, Link_Target);
    Ent.Link_Ok := Result = Directory.Link_Ok;

    -- Store target and its kind, rights and size
    if Show_Targets then
      -- Target is final target
      Ent.Link := Link_Target;
    end if;
    if Result = Directory.Link_Ok or else Result = Directory.Link_Recursive then
      -- Target is valid
      Stat := Sys_Calls.File_Stat (Link_Target.Image);
    else
      -- Defaults
      Stat := (Kind => Sys_Calls.Unknown, Rights => 0, Size => 0,
               others => <>);
    end if;
    Ent.Link_Kind := Directory.File_Kind_List (Stat.Kind);
    Ent.Link_Rights := Stat.Rights;
    if Show_Targets then
      Ent.Size := Stat.Size;
    end if;
  end Read_Link;

  -- Add a file if it matches
  procedure List (Ent_List : in out Entities.Entity_List;
                  File : in String) is
    Ent : Entities.Entity;
    Stat : Sys_Calls.File_Stat_Rec;
    use type Directory.File_Kind_List, Sys_Calls.Time_T;
  begin
    -- Prepare list for appending
    Ent_List.Rewind (Entities.Entity_List_Mng.Prev, False);

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
    if not Utc then
      -- Store modif time in local time
      Stat.Modif_Time := Stat.Modif_Time
                       + Sys_Calls.Gmt_Offset (Stat.Modif_Time);
    end if;
    Ent.Modif_Time := Sys_Calls.Tm_To_Time (Stat.Modif_Time);
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
    Loc_Dir : constant String := (if Dir = "" then "." else Dir);
    One_Listed : Boolean;
    Desc : Directory.Dir_Desc;
    Ent : Entities.Entity;
    Stat : Sys_Calls.File_Stat_Rec;
    use type Directory.File_Kind_List, Entities.Dots_Kind_List,
             Sys_Calls.Time_T;
  begin
    -- Prepare list for appending
    Ent_List.Rewind (Entities.Entity_List_Mng.Prev, False);
    -- Init Ent with path
    Ent.Path := As.U.Tus (Dir);
    -- Open
    begin
      Desc.Open (Loc_Dir);
    exception
      when Directory.Name_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": No such file or directory.");
        Exit_Code.Update (Exit_Code.Error);
        return;
      when Directory.Access_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Loc_Dir & ": Permission denied.");
        Exit_Code.Update (Exit_Code.Error);
        return;
    end;

    -- For each entry
    One_Listed := False;
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
             Directory.Build_File_Name (Loc_Dir, Ent.Name.Image, ""));
        exception
          when Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
            -- Skip this file
            raise Discard;
        end;

        -- Fill entity
        Ent.Kind := Directory.File_Kind_List (Stat.Kind);
        if not Utc then
          -- Store modif time in local time
          Stat.Modif_Time := Stat.Modif_Time
                           + Sys_Calls.Gmt_Offset (Stat.Modif_Time);
        end if;
        Ent.Modif_Time := Sys_Calls.Tm_To_Time (Stat.Modif_Time);
        Ent.Rights := Stat.Rights;
        Ent.User_Id := Stat.User_Id;
        Ent.Group_Id := Stat.Group_Id;
        Ent.Size := Stat.Size;
        Ent.Link.Set_Null;
        if Ent.Kind = Directory.Link then
          Read_Link (Directory.Build_File_Name (Loc_Dir, Ent.Name.Image, ""),
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
        -- At least one entity listed (so add current dir size)
        One_Listed := True;
      exception
        when Discard =>
          -- Discard this file
          null;
      end;

    end loop;

    -- Add current dir size
    if Total_Active and then Count_Dot and then One_Listed then
      Stat := Sys_Calls.File_Stat (Loc_Dir);
      Add_Size (Stat.Size, False);
    end if;

    -- Done
    Desc.Close;
  end List;

  -- Add a dir match or exclude template or regex
  -- Dir will match if no matching template or if it matches one of the
  --  matching templates, and if it does not match any exclude or discard
  --  template. It will be discarded (excluded AND subdirs not scanned)
  --  if it matches a discard template
  Dir_Match : Tmpl_List;
  Dir_Exclude : Tmpl_List;
  Dir_Discard : Tmpl_List;
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

  procedure Add_Dir_Discard (Template : in String; Regex : in Boolean) is
  begin
    if Check_Template (Template, Regex) then
      Dir_Discard.Insert ((As.U.Tus (Template), Regex));
    end if;
  end Add_Dir_Discard;

  -- Does a dir (full path) match
  function Dir_Matches (Dir : String) return Trilean.Trilean is
    Tmpl : Tmpl_Rec;
    Moved : Boolean;
  begin
    -- Check versus discarding templates
    if not Dir_Discard.Is_Empty then
      Dir_Discard.Rewind;
      loop
        Dir_Discard.Read (Tmpl, Moved => Moved);
        if Match (Dir, Tmpl.Template.Image, Tmpl.Regex) then
          -- The dir matches this discaring template
          return Discard;
        end if;
        exit when not Moved;
      end loop;
    end if;
    -- Check versus exclusion templates
    if not Dir_Exclude.Is_Empty then
      Dir_Exclude.Rewind;
      loop
        Dir_Exclude.Read (Tmpl, Moved => Moved);
        if Match (Dir, Tmpl.Template.Image, Tmpl.Regex) then
          -- The dir matches this exclusion template
          return Trilean.False;
        end if;
        exit when not Moved;
      end loop;
    end if;
    if not Dir_Match.Is_Empty then
      -- Check versus matching templates
      Dir_Match.Rewind;
      loop
        Dir_Match.Read (Tmpl, Moved => Moved);
        if Match (Dir, Tmpl.Template.Image, Tmpl.Regex) then
          -- The dir matches this matching template
          return Trilean.True;
        end if;
        exit when not Moved;
      end loop;
      -- The dir does not match any of the matching templates
      return Trilean.False;
    else
      -- Dir matches if no matching template
      return Trilean.True;
    end if;
  end Dir_Matches;

  -- List subdirs of Dir
  procedure Sort is new Str_List_Mng.Sort (As.U."<");
  procedure List_Dirs (Dir : in String;
                       List : out Dir_List) is
    Loc_Dir : constant String := (if Dir = "" then "." else Dir);
    Desc : Directory.Dir_Desc;
    Str : As.U.Asu_Us;
    use type Directory.File_Kind_List;
  begin
    -- Prepare list
    List.Delete_List (Deallocate => False);

    -- Open
    begin
      Desc.Open (Loc_Dir);
    exception
      when Directory.Name_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": No such file or directory.");
        Exit_Code.Update (Exit_Code.Error);
        return;
      when Directory.Access_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Loc_Dir & ": Permission denied.");
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

      -- Check if it is a directory
      declare
        Lstr : constant String := Str.Image;
        Fstr : constant String := Directory.Build_File_Name (Loc_Dir, Lstr, "");
        Kind : Directory.File_Kind_List;
        Link_Target : As.U.Asu_Us;
      begin
        if Lstr /= "."
        and then Lstr /= ".." then
          Kind := Directory.File_Kind (Fstr);
          if Kind = Directory.Dir then
            -- Directory: Append entity to list
            List.Insert (Str);
          elsif Follow_Links and then Kind = Directory.Link then
            -- Follow link
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
    -- Sort directories into alphabetic order
    Sort (List);
  end List_Dirs;

  -- Activate, then later get grand total
  procedure Activate_Total is
  begin
    Total_Active := True;
  end Activate_Total;

  function Get_Total return Size_Type is (Total_Size);

  function Get_Number return Natural is (Total_Number);

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

