with Basic_Proc, Environ, Xml_Parser;
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

  -- Load the conf
  Ctx : Xml_Parser.Ctx_Type;
  Root : Xml_Parser.Element_Type;
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
    Root := Ctx.Get_Root_Element;
  end Load;

  -- Editor GUI
  function Editor return String is
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 1), 1));
  end Editor;

  -- Diff GUI
  function Differator return String is
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    return Ctx.Get_Text (Ctx.Get_Child (Ctx.Get_Child (Root, 2), 1));
  end Differator;

  -- Bookmarks
  function Get_Bookmarks return Bookmark_Array is
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
    declare
      Result : Bookmark_Array (1 .. Ctx.Get_Nb_Children (Root) - 2);
    begin
      return Result;
    end;
  end Get_Bookmarks;

  procedure Del_Bookmark (Index : in Positive) is
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
  end Del_Bookmark;

  procedure Add_Bookmark (Bookmark : in String) is
  begin
    if not Xml_Parser.Is_Valid (Root) then
      Load;
    end if;
  end Add_Bookmark;

end Config;

