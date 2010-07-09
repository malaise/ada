with Basic_Proc, Environ, Xml_Parser.Generator;
package body Config is

  -- Config file name
  function File_Name return String is
  begin
    if not Environ.Is_Set ("HOME") then
      Basic_Proc.Put_Line_Error ("Env variable HOME not set.");
      raise Invalid_Config;
    end if;
    return Environ.Getenv ("HOME") & "/.agite/agite.xml";
  end File_Name;

  Curr_Dir_Pos : constant := 5;
  Bookmarks_Pos : constant := 6;

  -- Load the conf
  Ctx : Xml_Parser.Generator.Ctx_Type;
  Root : Xml_Parser.Element_Type;
  Bookmarks : Xml_Parser.Element_Type;
  procedure Load is
    use type Xml_Parser.Ctx_Status_List;
  begin
    if Ctx.Get_Status /= Xml_Parser.Clean then
      return;
    end if;
    -- Parse
    declare
      Ok : Boolean;
    begin
      Ctx.Parse (File_Name, Ok);
      if not Ok then
        Basic_Proc.Put_Line_Error ("Parse error in config: "
                                 & Ctx.Get_Parse_Error_Message);
        raise Invalid_Config;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Basic_Proc.Put_Line_Error ("Cannot open config file.");
        raise Invalid_Config;
    end;
    -- Store references
    Root := Ctx.Get_Root_Element;
    Bookmarks := Ctx.Get_Child (Root, Bookmarks_Pos);
    -- Verify that each definition has one (text) child
    -- Prev dir may be empty
    for I in 1 .. 3 loop
       if Ctx.Get_Nb_Children (Ctx.Get_Child (Root, I)) /= 1 then
         raise Invalid_Config;
      end if;
    end loop;
  end Load;

  -- X terminal
  function Xterminal return String is
  begin
    Load;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 1), 1));
  end Xterminal;

  -- Editor GUI
  function Editor return String is
  begin
    Load;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 2), 1));
  end Editor;

  -- Viewer GUI
  function Viewer return String is
  begin
    Load;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 3), 1));
  end Viewer;

  -- Diff GUI
  function Differator return String is
  begin
    Load;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 4), 1));
  end Differator;

  -- Last/Current dir
  procedure Save_Curr_Dir (Dir : in String) is
    Prev : Xml_Parser.Element_Type;
    New_Node : Xml_Parser.Node_Type;
  begin
    Load;
    -- Prev dir may not be empty
    Prev := Ctx.Get_Child (Root, Curr_Dir_Pos);
    if Ctx.Get_Nb_Children (Prev) = 1 then
      Ctx.Delete_Children (Prev);
    end if;
    Ctx.Add_Child (Prev, Dir, Xml_Parser.Text, New_Node);
    Ctx.Put (File_Name);
  end Save_Curr_Dir;

  function Prev_Dir return String is
    Prev : Xml_Parser.Element_Type;
  begin
    Load;
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
    Bookmark : Xml_Parser.Element_Type;
  begin
    Load;
    declare
      Result : Bookmark_Array (1 .. Ctx.Get_Nb_Children (Bookmarks));
    begin
      for I in Result'Range loop
        Bookmark := Ctx.Get_Child (Bookmarks, I);
        if Ctx.Get_Nb_Attributes (Bookmark) = 0 then
          Result(I).Name := Asu_Null;
        else
          Result(I).Name := Ctx.Get_Attribute (Bookmark, 1).Value;
        end if;
        if Ctx.Get_Nb_Children (Bookmark) /= 1 then
          -- No bookmark text: Separator
          Result(I).Path := Asu_Null;
        else
          -- Some bookmark text: full bookmark
          Result(I).Path := Asu_Us'(Ctx.Get_Text (Ctx.Get_Child (Bookmark, 1)));
        end if;
      end loop;
      return Result;
    end;
  end Get_Bookmarks;

  procedure Del_Bookmark (Index : in Positive) is
    Bookmark : Xml_Parser.Element_Type;
  begin
    Load;
    Bookmark := Ctx.Get_Child (Bookmarks, Index);
    -- Del Bookmark marker and its text
    Ctx.Delete_Node (Bookmark, Bookmark);
    Ctx.Put (File_Name);
  end Del_Bookmark;

  procedure Add_Bookmark (After_Index : in Natural; Bookmark : in String) is
    New_Node : Xml_Parser.Node_Type;
  begin
    Load;
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
    -- Add its text
    Ctx.Add_Child (New_Node, Bookmark, Xml_Parser.Text, New_Node);
    Ctx.Put (File_Name);
  end Add_Bookmark;

end Config;

