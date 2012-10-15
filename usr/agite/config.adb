with Basic_Proc, Environ, Directory, Xml_Parser.Generator, Timers;
package body Config is

  -- Config file name
  File_Path : As.U.Asu_Us;
  function Get_File_Name return String is
    Env_Agite_Conf_File : constant String := "AGITE_CONF_FILE";
  begin
    if File_Path.Is_Null then
      -- Build file name at fist call only
      -- Use $AGITE_CONF_FILE or default $HOME/.agite/agite.xml
      if Environ.Is_Set (Env_Agite_Conf_File) then
        File_Path := As.U.Tus (Environ.Getenv (Env_Agite_Conf_File));
      elsif Environ.Is_Set ("HOME") then
        File_Path := As.U.Tus (Environ.Getenv ("HOME") & "/.agite/agite.xml");
      else
        Basic_Proc.Put_Line_Error ("Env variables " & Env_Agite_Conf_File
                                 & " and HOME are both not set.");
        raise Invalid_Config;
      end if;
      -- Build full absolute path (agite is changing current dir)
      File_Path := As.U.Tus (Directory. Make_Full_Path (File_Path.Image));
    end if;
    return File_Path.Image;
  end Get_File_Name;

  Curr_Dir_Pos : constant := 7;
  Bookmarks_Pos : constant := 8;

  -- Load the conf and check
  Ctx : Xml_Parser.Generator.Ctx_Type;
  Root : Xml_Parser.Element_Type;
  Bookmarks : Xml_Parser.Element_Type;
  procedure Check is
    use type Xml_Parser.Ctx_Status_List;
  begin
    if Ctx.Get_Status /= Xml_Parser.Clean then
      return;
    end if;
    -- Parse
    declare
      Ok : Boolean;
    begin
      Ctx.Parse (Get_File_Name, Ok);
      if not Ok then
        Basic_Proc.Put_Line_Error ("Parse error in config: "
                                 & Ctx.Get_Parse_Error_Message);
        raise Invalid_Config;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Basic_Proc.Put_Line_Error (
            "Cannot open config file: " & Get_File_Name & ".");
        raise Invalid_Config;
    end;
    -- Store references
    Root := Ctx.Get_Root_Element;
    Bookmarks := Ctx.Get_Child (Root, Bookmarks_Pos);
    -- Verify that each definition has one (text) child
    -- Prev dir may be empty
    for I in 1 .. Curr_Dir_Pos - 1 loop
      if Ctx.Get_Nb_Children (Ctx.Get_Child (Root, I)) /= 1 then
        raise Invalid_Config;
      end if;
    end loop;
    -- Verify that bookmarks is named "bookmarks"
    if Ctx.Get_Name (Bookmarks) /= "bookmarks" then
      raise Invalid_Config;
    end if;
  end Check;

  -- Save the conf
  procedure Save is
    Ok : Boolean;
  begin
    -- Check Ctx, it is Ok for sure but this correctly sets
    --  Is_Mixed to False on inserted bookmarks
    Ctx.Check (Ok);
    if not Ok then
      Basic_Proc.Put_Line_Error ("Check error on config: "
                                 & Ctx.Get_Parse_Error_Message);
      raise Invalid_Config;
    end if;
    Ctx.Put (Get_File_Name);
  end Save;

  -- X terminal
  function Xterminal return String is
  begin
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 1), 1));
  end Xterminal;

  -- Editor GUI
  function Editor return String is
  begin
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 2), 1));
  end Editor;

  -- Viewer GUI
  function Viewer return String is
  begin
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 3), 1));
  end Viewer;

  -- Diff GUI
  function Differator return String is
  begin
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 4), 1));
  end Differator;

  -- Make command
  function Make return String is
  begin
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 5), 1));
  end Make;

  -- Refresh Period
  function Period return Duration is
    Result : Timers.Period_Range;
  begin
    Result := Timers.Period_Range'Value (
               Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 6), 1)));
    return Result;
  exception
    when others =>
      raise Invalid_Config;
  end Period;

  -- Last/Current dir
  procedure Save_Curr_Dir (Dir : in String) is
    Prev : Xml_Parser.Element_Type;
    New_Node : Xml_Parser.Node_Type;
  begin
    -- Prev dir may not be empty
    Prev := Ctx.Get_Child (Root, Curr_Dir_Pos);
    if Ctx.Get_Nb_Children (Prev) = 1 then
      Ctx.Delete_Children (Prev);
    end if;
    Ctx.Add_Child (Prev, Dir, Xml_Parser.Text, New_Node);
    Save;
  end Save_Curr_Dir;

  function Prev_Dir return String is
    Prev : Xml_Parser.Element_Type;
  begin
    -- Prev dir may be empty
    Prev := Ctx.Get_Child (Root, Curr_Dir_Pos);
    if Ctx.Get_Nb_Children (Prev) = 1 then
      return Ctx.Get_Text (Ctx.Get_Child (Prev, 1));
    else
      return "";
    end if;
  end Prev_Dir;

  -- Bookmarks
  function Get_Bookmarks return Bookmark_Array is
  begin
    declare
      Result : Bookmark_Array (1 .. Ctx.Get_Nb_Children (Bookmarks));
    begin
      for I in Result'Range loop
        Result(I) := Get_Bookmark (I);
      end loop;
      return Result;
    end;
  end Get_Bookmarks;

  function Get_Bookmark (Index : Positive) return Bookmark_Rec is
    Bookmark : Xml_Parser.Element_Type;
    Result : Bookmark_Rec;
  begin
    Bookmark := Ctx.Get_Child (Bookmarks, Index);
    if Ctx.Get_Nb_Attributes (Bookmark) = 0 then
      Result.Name.Set_Null;
    else
      Result.Name := Ctx.Get_Attribute (Bookmark, 1).Value;
    end if;
    if Ctx.Get_Nb_Children (Bookmark) /= 1 then
      -- No bookmark text: Separator
      Result.Path.Set_Null;
    else
      -- Some bookmark text: full bookmark
      Result.Path := As.U.Asu_Us'(Ctx.Get_Text (Ctx.Get_Child (Bookmark, 1)));
    end if;
    return Result;
  end Get_Bookmark;

  procedure Del_Bookmark (Index : in Positive) is
    Bookmark : Xml_Parser.Element_Type;
  begin
    Bookmark := Ctx.Get_Child (Bookmarks, Index);
    -- Del Bookmark marker and its text
    Ctx.Delete_Node (Bookmark, Bookmark);
    Save;
  end Del_Bookmark;

  procedure Add_Bookmark (After_Index : in Natural;
                          Bookmark : in Bookmark_Rec) is
    New_Node : Xml_Parser.Node_Type;
  begin
    -- Add Bookmark marker
    if After_Index = 0 then
      -- As first child
      Ctx.Add_Child (Bookmarks, "bookmark", Xml_Parser.Element,
                     New_Node, False);
    else
      -- After Index
      New_Node := Ctx.Get_Child (Bookmarks, After_Index);
      Ctx.Add_Brother (New_Node, "bookmark", Xml_Parser.Element,
                       New_Node, True);
    end if;
    -- Bookmarks can be empty (when separators)
    Ctx.Set_Put_Empty (New_Node, True);
    -- Add its name attribute
    if not Bookmark.Name.Is_Null then
      Ctx.Add_Attribute (New_Node, "Name", Bookmark.Name.Image);
    end if;
    -- Add its text
    if not Bookmark.Path.Is_Null then
      Ctx.Add_Child (New_Node, Bookmark.Path.Image, Xml_Parser.Text,
                     New_Node);
    end if;
    Save;
  end Add_Bookmark;

  procedure Move_Bookmark (Index : in Positive; Up : in Boolean) is
    Bookmark : Xml_Parser.Element_Type;
    Name, Path : As.U.Asu_Us;
  begin
    -- Move to bookmark at index
    Bookmark := Ctx.Get_Child (Bookmarks, Index);
    -- Read its name and path
    if Ctx.Get_Nb_Attributes (Bookmark) /= 0 then
      Name := Ctx.Get_Attribute (Bookmark, 1).Value;
    end if;
    if Ctx.Get_Nb_Children (Bookmark) /= 0 then
      Path := Ctx.Get_Text (Ctx.Get_Child (Bookmark, 1));
    end if;

    -- Delete this bookmark
    Ctx.Delete_Node (Bookmark, Bookmark);
    -- Insert after new index
    if Up then
      Add_Bookmark (Index - 2, (Name,Path));
    else
      Add_Bookmark (Index, (Name, Path));
    end if;
  end Move_Bookmark;

end Config;

