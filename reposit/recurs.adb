with My_Io, Text_Handler, Directory;
procedure Recurs (Name_Of_Dir : in Boolean := True;
                  In_Current : in Boolean := True;
                  First_Level_Only : in Boolean := False;
                  Leaves_Only : in Boolean := False;
                  Stop_On_Error : in Boolean := True;
                  Follow_Links : in Boolean := False) is

  Current_Level : Natural := 0;
  Abort_Explore : exception;
  Dot_Dir     : constant String := ".";
  Dot_Dot_Dir : constant String := "..";

  subtype Dir_Txt is Text_Handler.Text (Directory.Max_Dir_Name_Len);

  procedure Explore (Curr_Name : in String) is
    Dir_Dsc : Directory.Dir_Desc;
    Full_Curr_Name, New_Name : Dir_Txt;
    Kind : Directory.File_Kind_List;
    Rights : Natural;
    Mtime : Directory.Time_T;
    Nb_Sons : Natural;
    use Directory;

    procedure Do_Here is
    begin
      -- Display current drive and directory
      if Name_Of_Dir then
        My_Io.New_Line;
        My_Io.Put ("==> ");
        My_Io.Put (Text_Handler.Value(Full_Curr_Name));
        My_Io.Put_Line (" <==");
      end if;

      if not Do_In_Dir and then Stop_On_Error then
        My_Io.Put_Line (" *** Abort ***");
        raise Abort_Explore;
      end if;
    end Do_Here;

  begin

    -- Go in current directory
    begin
      Directory.Change_Current (Curr_Name);
    exception
      when Directory.Name_Error | Directory.Access_Error =>
        My_Io.New_Line;
        My_Io.Put ("Error changing to directory " & Curr_Name);
        if Stop_On_Error then
          My_Io.Put_Line (" *** Abort ***");
          raise Abort_Explore;
        else
           My_Io.New_Line;
           return;
        end if;
    end;

    Directory.Get_Current (Full_Curr_Name);

    -- Do current dir when not Leaves_Only
    if not Leaves_Only then
      if Current_Level /= 0 or else In_Current then
        -- Check if do action in intial dir
        Do_Here;
      end if;
      -- Optim: Done it if first level only
      if Current_Level = 1 and then First_Level_Only then
        return;
      end if;
    end if;

    -- Go to next sub dir
    Nb_Sons := 0;
    Dir_Dsc := Directory.Open(Text_Handler.Value(Full_Curr_Name));
    loop
      begin
        Directory.Next_Entry (Dir_Dsc, New_Name);
      exception
        when Directory.End_Error =>
          exit;
      end;
      begin
        Directory.File_Stat (Text_Handler.Value(New_Name), Kind, Rights, Mtime);
      exception
        when Directory.Name_Error | Directory.Access_Error =>
          -- A link to nowhere?
          Kind := Directory.Unknown;
      end;
      -- Follow link recursively
      if Follow_Links and then Kind = Directory.Link then
        begin
          Directory.Read_Link (Text_Handler.Value(New_Name), New_Name, True);
          Directory.File_Stat (Text_Handler.Value(New_Name), Kind, Rights,
                               Mtime);
        exception
          when Directory.Name_Error | Directory.Access_Error =>
            -- A link to nowhere?
            Kind := Directory.Unknown;
        end;
      end if;
        
      if Kind = Directory.Dir
      and then Text_Handler.Value(New_Name) /= Dot_Dir 
      and then Text_Handler.Value(New_Name) /= Dot_Dot_Dir then
        -- Restart with next son if not First_Level_Only
        if Current_Level /= 1 or else not First_Level_Only then
          Current_Level := Current_Level + 1;
          Explore (Text_Handler.Value(New_Name));
          Current_Level := Current_Level - 1;
          Directory.Change_Current (Text_Handler.Value(Full_Curr_Name));
        end if;
        Nb_Sons := Nb_Sons + 1;
      end if;
    end loop;
    Directory.Close(Dir_Dsc);


    -- When Leaves_Only, do current dir after counting sons
    if Nb_Sons = 0 and then Leaves_Only then
      -- Check if do action in current dir
      if Current_Level /= 0 or else In_Current then
        Do_Here;
      end if;
    end if;

  exception
    when Abort_Explore =>
      if Current_Level = 0 then
        -- Back to start directory
        Directory.Change_Current (Text_Handler.Value(Full_Curr_Name));
      else
        Current_Level := Current_Level - 1;
        raise;
      end if;
  end Explore;

begin -- Recurs
  Explore (Directory.Get_Current);
end Recurs;

