with Ada.Strings.Unbounded;
with Xml_Parser, Environ;
separate (Def_Enigma)
package body Xml is

  -- Ada.Strings.Unbounded and Ada.Exceptions re-definitions
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String renames Asu.To_String;

  -- The configuration file name.
  Default_File_Name : constant String := "enigma.xml";
  File_Var_Name : constant String := "ENIGMA_CONF_FILE";

  -- The Xml context and root node
  Ctx : Xml_Parser.Ctx_Type;
  Root : Xml_Parser.Element_Type;

  procedure Init is
    -- Return the configuration file name
    function File_Name return String is
    begin
      if not Environ.Is_Set (File_Var_Name)
      or else Environ.Getenv (File_Var_Name) /= "" then
        return Default_File_Name;
      else
        return Environ.Getenv (File_Var_Name);
      end if;
    end File_Name;
    Parse_Ok : Boolean;
  begin
    if Xml_Parser.Is_Valid (Root) then
      return;
    end if;
    -- Parse Xml
    begin
      Ctx.Parse (File_Name, Parse_Ok);
      if not Parse_Ok then
        Sys_Calls.Put_Line_Error ("ERROR in file " & File_Name
                                & ": " & Ctx.Get_Parse_Error_Message);
        raise Invalid_Configuration;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Sys_Calls.Put_Line_Error ("ERROR: File " & File_Name & " not found.");
        raise Invalid_Configuration;
    end;
    Root := Ctx.Get_Root_Element;
  end Init;

  -- Get the Name of a rotor of reflector, given its id. "" if not found
  function Get_Name (Rotor : Boolean; Id : Positive) return String is
    Children : constant Xml_Parser.Nodes_Array := Ctx.Get_Children (Root);
    Attrs : Xml_Parser.Attributes_Array (1 ..2);
    Attr : Xml_Parser.Attribute_Rec;
    Index : Natural;
  begin
    Index := 0;
    for I in Children'Range loop
      if Rotor and then Ctx.Get_Name (Children(I)) = "Rotor" then
        Index := Index + 1;
        Attrs := Ctx.Get_Attributes (Children(I));
        if Asu_Ts (Attrs(1).Name) = "Name" then
          Attr := Attrs(1);
        else
          Attr := Attrs(2);
        end if;
      elsif not Rotor and then Ctx.Get_Name (Children(I)) = "Reflector" then
        Index := Index + 1;
        Attr := Ctx.Get_Attribute (Children(I), 1);
      end if;
      if Index = Id then
        return Asu_Ts (Attr.Value);
      end if;
    end loop;
    return "";
  end Get_Name;

  -- Get the Id of a rotor of reflector, given its name. 0 if not found
  function Get_Id (Rotor : Boolean; Name : String) return Natural is
    Children : constant Xml_Parser.Nodes_Array := Ctx.Get_Children (Root);
    Attrs : Xml_Parser.Attributes_Array (1 ..2);
    Attr : Xml_Parser.Attribute_Rec;
    Index : Natural;
    Match : Boolean;
  begin
    Index := 0;
    for I in Children'Range loop
      Match := True;
      if Rotor and then Ctx.Get_Name (Children(I)) = "Rotor" then
        Index := Index + 1;
        Attrs := Ctx.Get_Attributes (Children(I));
        if Asu_Ts (Attrs(1).Name) = "Name" then
          Attr := Attrs(1);
        else
          Attr := Attrs(2);
        end if;
      elsif not Rotor and then Ctx.Get_Name (Children(I)) = "Reflector" then
        Index := Index + 1;
        Attr := Ctx.Get_Attribute (Children(I), 1);
      else
        Match := False;
      end if;
      if Match and then Asu_Ts (Attr.Value) = Name then
        return Index;
      end if;
    end loop;
    return 0;
  end Get_Id;

end Xml;

