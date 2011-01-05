with Xml_Parser;
package body Byte_To_Unicode is


  function Value (Str : String) return Natural is
  begin
    if Str'Length >= 2 and then Str (Str'First .. Str'First + 1) = "0x" then
      declare
        New_Str : constant String
                := "16#" & Str (Str'First + 2 .. Str'Last) & "#";
      begin
        return Natural'Value (New_Str);
      end;
    else
      return Natural'Value (Str);
    end if;
  end Value;

  procedure Load (The_Map : out Map; File_Name : in String) is
    Ctx : Xml_Parser.Ctx_Type;
    Ok : Boolean;
    Set : array (Byte'Range) of Boolean
        := (others => False);
    Code : Byte;
    Node : Xml_Parser.Element_Type;
    Child : Xml_Parser.Node_Type;
    Attr : Xml_Parser.Attribute_Rec;
  begin
    -- Parse the file
    Ctx.Parse (File_Name, Ok);
    if not Ok then
      raise Parse_Error;
    end if;

    -- Get the Map and check Nb of codes
    Node := Ctx.Get_Root_Element;
    if Ctx.Get_Nb_Children (Node) /= Table_Array'Length then
      raise Parse_Error;
    end if;

    -- Iterate on all children of the Map
    for I in Table_Array'Range loop
      Child := Ctx.Get_Child (Node, I+1);
      -- Get code, first attribute
      Attr := Ctx.Get_Attribute (Child, 1);
      Code := Value (Attr.Value.Image);
      if Set(Code) then
        -- This Code already set
        raise Parse_Error;
      end if;
      -- Get Unicode: Text
      Child := Ctx.Get_Child (Child, 1);
      The_Map.Table(Code) := Value (Ctx.Get_Text (Child));
      Set(Code) := True;
    end loop;

  exception
    when Xml_Parser.File_Error =>
      The_Map.Table := (others => 0);
      raise File_Error;
    when others =>
      The_Map.Table := (others => 0);
      raise Parse_Error;
  end Load;

  -- Returns the Unicode corresponding to a given byte in the table
  function Convert (The_Map : Map;
                    Code : Byte) return Unicode_Number is
  begin
    return The_Map.Table (Code);
  end Convert;

end Byte_To_Unicode;

