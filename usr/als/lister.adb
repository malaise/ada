with Ada.Calendar;
with Basic_Proc, Argument, Sys_Calls;
package body Lister is

  -- type Dots_Kind_List is (Basic, Basic_Dots, Basic_Dots_Roots);
  package Asu renames Ada.Strings.Unbounded;

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
                  Only_Dirs : Boolean;
                  Only_Links : Boolean;
                  Only_Files : Boolean) return Boolean is
    use type Directory.File_Kind_List;
  begin
    if not (Only_Dirs or else Only_Links or else Only_Files) then
      -- No criteria => averything matches
      return True;
    end if;
    if Kind = Directory.Dir then
      return Only_Dirs;
    elsif Kind = Directory.Link then
      return  Only_Links;
    elsif Kind = Directory.File then
      return Only_Files;
    else
      -- Any other kind
      return False;
    end if;
  end Match;

  -- Add a file if it matches
  procedure List (Ent_List : in out Entities.Entity_List;
                  File : in String;
                  Date1, Date2 : in Entities.Date_Spec_Rec) is
    Ent : Entities.Entity;
    Stat : Sys_Calls.File_Stat_Rec;
    use type Directory.File_Kind_List;
  begin
    -- Prepare list for appending
    if not Ent_List.Is_Empty then
       Ent_List.Rewind (Entities.Entity_List_Mng.Prev);
    end if;

    -- Read file stat
    begin
      Stat := Sys_Calls.File_Stat (File);
    exception
      when Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
        -- Skip this file
        return;
    end;

    -- Check date
    Ent.Modif_Time := Sys_Calls.Time_Of (Stat.Modif_Time);
    if not Match (Ent.Modif_Time, Date1)
    or else not Match (Ent.Modif_Time, Date2) then
      return;
    end if;

    -- Fill entity
    Ent.Name := Asu.To_Unbounded_String (Directory.Basename(File));
    Ent.Path := Asu.To_Unbounded_String (Directory.Dirname(File));
    Ent.Kind := Directory.File_Kind_List (Stat.Kind);
    Ent.Rights := Stat.Rights;
    Ent.User_Id := Stat.User_Id;
    Ent.Group_Id := Stat.Group_Id;
    Ent.Size := Stat.Size;
    Ent.Link := Asu.Null_Unbounded_String;
    if Ent.Kind = Directory.Link then
      -- Read link
      begin
        Ent.Link := Asu.To_Unbounded_String (Directory.Read_Link (
            File_Name => File, Recursive => False));
      exception
        when Directory.Name_Error | Directory.Access_Error =>
          Ent.Link := Asu.Null_Unbounded_String;
      end;
    end if;
    -- Insert entity
    Ent_List.Insert (Ent);
  end List;

  -- List content of Dir, possibly dots, matching criteria
  procedure List (Ent_List : in out Entities.Entity_List;
                  Dir : in String;
                  Dots : in Entities.Dots_Kind_List;
                  Only_Dirs : in Boolean;
                  Only_Links : in Boolean;
                  Only_Files : in Boolean;
                  Date1, Date2 : in Entities.Date_Spec_Rec) is
    Desc : Directory.Dir_Desc;
    Ent : Entities.Entity;
    Modif_Time : Ada.Calendar.Time;
    Stat : Sys_Calls.File_Stat_Rec;
    use type Directory.File_Kind_List, Entities.Dots_Kind_List;
  begin
    -- Prepare list for appending
    if not Ent_List.Is_Empty then
       Ent_List.Rewind (Entities.Entity_List_Mng.Prev);
    end if;
    -- Init Ent with path
    Ent.Path := Asu.To_Unbounded_String (Dir);
    -- Open
    begin
      Desc := Directory.Open (Dir);
    exception
      when Directory.Name_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": No such file or directory.");
        return;
      when Directory.Access_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": Permission denied.");
        return;
    end;

    -- For each entry
    loop
      declare
        Discard : exception;
      begin
        -- Read next entry
        begin
          Ent.Name := Asu.To_Unbounded_String (Directory.Next_Entry (Desc));
        exception
          when Directory.End_Error =>
            -- Done
            exit;
        end;

        -- Check if file name matches dot criteria
        declare
          Str : constant String := Asu.To_String (Ent.Name);
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
             Directory.Build_File_Name (Dir, Asu.To_String (Ent.Name), ""));
        exception
          when Sys_Calls.Name_Error | Sys_Calls.Access_Error =>
            -- Skip this file
            raise Discard;
        end;

        -- Check kind versus criteria
        Ent.Kind := Directory.File_Kind_List (Stat.Kind);
        if not Match (Ent.Kind, Only_Dirs, Only_Links, Only_Files) then
          raise Discard;
        end if;

        -- Check modif time versus criteria
        Modif_Time := Sys_Calls.Time_Of (Stat.Modif_Time);
        if not Match (Modif_Time, Date1)
        or else not Match (Modif_Time, Date2) then
          raise Discard;
        end if;

        -- Fill entity
        Ent.Rights := Stat.Rights;
        Ent.User_Id := Stat.User_Id;
        Ent.Group_Id := Stat.Group_Id;
        Ent.Modif_Time := Modif_Time;
        Ent.Size := Stat.Size;
        Ent.Link := Asu.Null_Unbounded_String;
        if Ent.Kind = Directory.Link then
          -- Read link
          begin
            Ent.Link := Asu.To_Unbounded_String (Directory.Read_Link (
                File_Name => Directory.Build_File_Name (Dir,
                                    Asu.To_String (Ent.Name), ""),
                Recursive => False));
          exception
            when Directory.Name_Error | Directory.Access_Error =>
              Ent.Link := Asu.Null_Unbounded_String;
          end;
        end if;

        -- Append entity to list
        Ent_List.Insert (Ent);
      exception
        when Discard =>
          -- Discard this file
          null;
      end;

    end loop;

    -- Done
    Directory.Close (Desc);
  end List;

  -- List subdirs of Dir
  procedure List_Dirs (Dir : in String;
                       List : out Dir_List) is
    Desc : Directory.Dir_Desc;
    Str : Asu.Unbounded_String;
    use type Directory.File_Kind_List;
  begin
    -- Prepare list
    List.Delete_List (Deallocate => False);

    -- Open
    begin
      Desc := Directory.Open (Dir);
    exception
      when Directory.Name_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": No such file or directory.");
        return;
      when Directory.Access_Error =>
        Basic_Proc.Put_Line_Error (Argument.Get_Program_Name & ": "
                                 & Dir & ": Permission denied.");
        return;
    end;

    -- For each entry
    loop
      begin
        Str := Asu.To_Unbounded_String (Directory.Next_Entry (Desc));
      exception
        when Directory.End_Error =>
          -- Done
          exit;
      end;

      -- Check if it is a directory
      if Asu.To_String (Str) /= "."
      and then Asu.To_String (Str) /= ".."
      and then Directory.File_Kind (
          Directory.Build_File_Name (Dir, Asu.To_String (Str), ""))
             = Directory.Dir then
        -- Append entity to list
        List.Insert (Str);
      end if;
    end loop;

    -- Done
    Directory.Close (Desc);
  end List_Dirs;

end Lister;


