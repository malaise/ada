separate (Xml_Parser.Parse_Mng)

package body Namespaces is
  Xml : constant String := "xml";
  Xml_Prefix : constant String := "xml:";

  Xmlns : constant String := "xmlns";
  Xmlns_Prefix : constant String := "xmlns:";

  Xml_Domain   : constant String := "http://www.w3.org/XML/1998/namespace";
  Xmlns_Domain : constant String := "http://www.w3.org/2000/xmlns/";

  -- Init once
  procedure Init (Ctx : in out Ctx_Type) is
  begin
    if Ctx.Namespace_List.Is_Empty then
      -- Add definitions of xml and xmlns namespaces
      Ctx.Namespace_List.Insert (
        (As.U.Tus (Xml), As.U.Tus (Xml_Domain)), Namespace_List_Mng.First);
      Ctx.Namespace_List.Insert (
        (As.U.Tus (Xmlns), As.U.Tus (Xmlns_Domain)), Namespace_List_Mng.First);
    end if;
  end Init;

  -- Validate that name has 0 or 1 ':' if Elt or Attr
  --  or has no ':' otherwise
  -- ':' must not start/end the name
  -- Elt name
  procedure Validate (Ctx : in out Ctx_Type;
                      Name : in As.U.Asu_Us;
                      Elt_Attr : in Elt_Attr_List) is
    Occurence : Positive;
    Index : Natural;
  begin
    Occurence :=
        (if Elt_Attr /= Other
         or else (Name.Length > Xml_Prefix'Length
                  and then Name.Slice (1, Xml_Prefix'Length) = Xml_Prefix) then
           -- Element or CDATA or Enumeration or NMTOKEN(s) or "xml:xxx"
           -- There can be 0 or 1 occurence, not 2, in element or attribute name
           2
         else
           -- There cannnot be even 1 occurence, in others
           1);

    -- If too many ':' => error, if none => ok
    if Str_Util.Locate (Name.Image, ":", Occurence => Occurence) /= 0 then
      Util.Error (Ctx.Flow, "Invalid qualified name " & Name.Image);
    elsif Elt_Attr = Other then
      return;
    end if;

    -- There is only one ":", must not start/end the name
    Index := Str_Util.Locate (Name.Image, ":");
    if Index = 1 or else Index = Name.Length then
      Util.Error (Ctx.Flow, "Invalid qualified name " & Name.Image);
    end if;

    -- Element cannot have prefix xmlns
    if Elt_Attr = Elt and then
    Index = 6 and then Name.Slice (1, 6) = Xmlns_Prefix then
      Util.Error (Ctx.Flow, "Invalid element prefix " & Name.Image);
    end if;
  end Validate;

  procedure Validate_Definition (Ctx : in out Ctx_Type;
                                 Name, Namespace : in As.U.Asu_Us) is
    Ind : Natural;
  begin
    Ind := Str_Util.Locate (Name.Image, ":");
    -- xmlns:xml
    if Name.Image = Xmlns_Prefix & Xml then
      if Namespace.Image /= Xml_Domain then
        -- "xmlns:xml" must bind to Xml_Domain
        Util.Error (Ctx.Flow, "Invalid redifinition of xml namespace "
                            & Namespace.Image);
      end if;
    elsif Ind = 6 and then Name.Slice (1, 6) = Xmlns_Prefix
    and then Namespace.Image = Xml_Domain then
      -- Other namespace definition must not bind to xml domain
      Util.Error (Ctx.Flow, "Invalid binding of " & Name.Image
                          & " to xml namespace.");
    end if;

    -- xmlns:xmlns
    if Name.Image = Xmlns_Prefix & Xmlns then
      -- "xmlns:xmlns" cannot be declared
      Util.Error (Ctx.Flow, "Invalid redifinition of xmlns namespace "
                          & Namespace.Image);
    elsif Ind = 6 and then Name.Slice (1, 6) = Xmlns_Prefix
    and then Namespace.Image = Xmlns_Domain then
      -- Other namespace definition must not by to xmlns domain
      Util.Error (Ctx.Flow, "Invalid binding of " & Name.Image
                          & "to xmlns namespace.");
    end if;

    -- Xml and Xmlns domain not as default
    if Name.Image = Xmlns and then
    (Namespace.Image = Xml_Domain or else Namespace.Image = Xmlns_Domain) then
      Util.Error (Ctx.Flow, "Invalid default namespace " & Namespace.Image);
    end if;

    -- xmlns: Namespace name cannot be empty
    if Name.Image = Xmlns
    or else (Ind = 6 and then Name.Slice (1, 6) = Xmlns_Prefix) then
      if Namespace.Is_Null then
        Util.Error (Ctx.Flow, "Invalid empty namespace definition for "
                            & Name.Image);
      end if;
    end if;
  end Validate_Definition;

  -- Add / overwrite the definition of Name if Name is xmlns[:<suffix>]
  -- Empty Prefix means default namespace
  procedure Add (Ctx : in out Ctx_Type;
                 Name, Namespace : in As.U.Asu_Us) is
    Item : Namespace_Type;
  begin
    if Name.Length > Xmlns_Prefix'Length
    and then Name.Slice (1, Xmlns_Prefix'Length) = Xmlns_Prefix then
      -- Keep suffix
      Item.Prefix := Name.Uslice (Xmlns_Prefix'Length + 1, Name.Length);
    elsif Name.Image = Xmlns then
      -- Default domain
      Item.Prefix.Set_Null;
    else
      -- Not a Namespace definition
      return;
    end if;
    Validate_Definition (Ctx, Name, Namespace);

    -- Insert or replace
    Item.Namespace := Namespace;
    Debug ("    Adding namespace " & Item.Prefix.Image
         & " -> " & Item.Namespace.Image);
    Ctx.Namespace_List.Insert (Item, Namespace_List_Mng.First);
  end Add;

   -- Delete Namespace
  procedure Del (Ctx : in out Ctx_Type;
                 Name : in As.U.Asu_Us) is
    Item : Namespace_Type;
    Found : Boolean;
  begin
    if Name.Length > Xmlns_Prefix'Length
    and then Name.Slice (1, Xmlns_Prefix'Length) = Xmlns_Prefix then
      -- Keep suffix
      Item.Prefix := Name.Uslice (Xmlns_Prefix'Length + 1, Name.Length);
    elsif Name.Image = Xmlns then
      -- Default domain
      Item.Prefix.Set_Null;
    else
      -- Not a Namespace definition
      return;
    end if;
    -- Delete first item with this preofix
    Ctx.Namespace_List.Search_First (Item, Found, Namespace_List_Mng.Forward);
    if Found then
      if Debug_On then
        Ctx.Namespace_List.Read_Current (Item);
        Debug ("    Deleting namespace " & Item.Prefix.Image
             & " -> " & Item.Namespace.Image);
      end if;
      Ctx.Namespace_List.Delete_Current;
    else
      Debug ("Deleting Unknonw namespace " & Name.Image);
      raise Internal_Error;
    end if;
  end Del;

  -- Get the Namespace of name.
  -- Return default if Element is not qualified
  function Get (Ctx : in out Ctx_Type;
                Name : in As.U.Asu_Us;
                Element : in Boolean) return As.U.Asu_Us is
    Index : Natural;
    Item : Namespace_Type;
    Found : Boolean;
  begin
    -- Don't resolve a declaration of NS
    Index := Str_Util.Locate (Name.Image, ":");
    if Name.Image = Xmlns
    or else (Index = Xmlns_Prefix'Length
           and then Name.Slice(1, Xmlns_Prefix'Length) = Xmlns_Prefix) then
      return As.U.Asu_Null;
    end if;
    if Index = 0 then
      -- Not qualified => Default namespace
      if Element then
        -- Default namespace for elements
        Item.Prefix.Set_Null;
      else
        -- No default for attributes
        return As.U.Asu_Null;
      end if;
    else
      -- Extract prefix
      Item.Prefix := Name.Uslice (1, Index - 1);
    end if;
    -- Read namespace
    Ctx.Namespace_List.Search_First (Item, Found, Namespace_List_Mng.Forward);
    if Found then
      Ctx.Namespace_List.Read_Current (Item);
      Debug ("    Got namespace " & Name.Image & " -> " & Item.Namespace.Image);
      return Item.Namespace;
    elsif Index = 0 then
      -- No default namespace
      return As.U.Asu_Null;
    else
      Util.Error (Ctx.Flow, "Unknown namespace " & Name.Image);
      return As.U.Asu_Null;
    end if;
  end Get;

end Namespaces;

