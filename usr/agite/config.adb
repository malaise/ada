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

  Bookmarks_Pos : constant := 4;

  -- Load the conf
  Ctx : Xml_Parser.Generator.Ctx_Type;
  Root : Xml_Parser.Element_Type;
  Bookmarks : Xml_Parser.Element_Type;
  procedure Load is
  begin
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
    for I in 1 .. 3 loop
       if Ctx.Get_Nb_Children (Ctx.Get_Child (Root, I)) /= 1 then
         raise Invalid_Config;
      end if;
    end loop;
  end Load;

  -- Editor GUI
  function Editor return String is
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 1), 1));
  end Editor;

  -- Viewer GUI
  function Viewer return String is
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 2), 1));
  end Viewer;

  -- Diff GUI
  function Differator return String is
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 3), 1));
  end Differator;

  -- Bookmarks
  function Get_Bookmarks return Bookmark_Array is
    Bookmark : Xml_Parser.Element_Type;
    use type Utils.Asu_Us;
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    declare
      Result : Bookmark_Array (1 .. Ctx.Get_Nb_Children (Bookmarks));
      Name : Utils.Asu_Us;
    begin
      for I in Result'Range loop
        Bookmark := Ctx.Get_Child (Bookmarks, I);
        if Ctx.Get_Nb_Attributes (Bookmark) = 0 then
          Name := Utils.Asu_Null;
        else
          Name := "(" & Ctx.Get_Attribute (Bookmark, 1).Value & ")";
        end if;
        if Ctx.Get_Nb_Children (Bookmark) /= 1 then
          -- No bookmark text: Separator
          Result(I) := "----- " & Name & " -----";
        else
          if Name /= Utils.Asu_Null then
            Name := Name & " ";
          end if;
          -- Some bookmark text: full bookmark
          Result(I) := Name & Utils.Asu_Us'
               (Ctx.Get_Text (Ctx.Get_Child (Bookmark, 1)));
        end if;
      end loop;
      return Result;
    end;
  end Get_Bookmarks;

  procedure Del_Bookmark (Index : in Positive) is
    Bookmark : Xml_Parser.Element_Type;
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    Bookmark := Ctx.Get_Child (Bookmarks, Index);
    -- Del Bookmark marker and its text
    Ctx.Delete_Node (Bookmark, Bookmark);
    Ctx.Put (File_Name);
    Root := Xml_Parser.No_Node;
    Ctx.Clean;
  end Del_Bookmark;

  procedure Add_Bookmark (Bookmark : in String) is
    New_Node : Xml_Parser.Node_Type;
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    -- Add Bookmark marker and its text
    Ctx.Add_Child (Bookmarks, "bookmark", Xml_Parser.Element, New_Node);
    Ctx.Add_Child (New_Node, Bookmark, Xml_Parser.Text, New_Node);
    Ctx.Put (File_Name);
    Root := Xml_Parser.No_Node;
    Ctx.Clean;
  end Add_Bookmark;

end Config;

