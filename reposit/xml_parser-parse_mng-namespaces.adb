separate (Xml_Parser.Parse_Mng)

package body Namespaces is
  -- Validate that name has 0 or 1 ':' if Elt or Attr
  --  or has no ':' otherwise
  function Valid (Name : in As.U.Asu_Us;
                  Elt_Attr : in Boolean) return Boolean is
    Occurence : Positive;
  begin
    if Elt_Attr then
      -- There can be 0 or 1 occurence, not 2, in element or attribute name
      Occurence := 2;
    else
      -- There cannnot be even 1 occurence, in others
      Occurence := 1;
    end if;
    return String_Mng.Locate (Name.Image, ":", Occurence => Occurence) = 0;
  end Valid;

  Xmlns : constant String := "xmlns";
  Xmlns_Prefix : constant String := "xmlns:";

  -- Add / overwrite the definition of Name if Name is xmlns[:<suffix>]
  -- Empty Prefix means default namespace
  -- Delete it if Namespace is empty
  procedure Add (Ctx : in out Ctx_Type;
                 Name, Namespace : in As.U.Asu_Us) is
    Item : Namespace_Type;
    Done : Boolean;
  begin
    if Name.Length > Xmlns_Prefix'Length
    and then Name.Slice (1, Xmlns_Prefix'Length) = Xmlns_Prefix then
      -- Keep suffix
      Item.Prefix := Name.Uslice (Xmlns_Prefix'Length + 1, Item.Prefix.Length);
    elsif Name.Image /= Xmlns then
      -- Not a Namespace => exit, otherwise default (empty) domain
      return;
    end if;
    if Namespace.Is_Null then
      -- Delete if exists
      Trace ("    Deleting namespace " & Item.Prefix.Image);
      Ctx.Namespace_List.Delete (Item, Done);
    else
      -- Insert or replace
      Item.Namespace := Namespace;
      Trace ("    Adding namespace " & Item.Prefix.Image
           & " -> " & Item.Namespace.Image);
      Ctx.Namespace_List.Insert (Item);
    end if;
  end Add;

  -- Get the Namespace of name.
  -- Return default if Element not qualified
  -- return empty if Attribute not qualified or prefix unknown
  procedure Get (Ctx : in out Ctx_Type;
                 Name : in As.U.Asu_Us;
                 Element : in Boolean;
                 Namespace : out As.U.Asu_Us) is
    Index : Natural;
    Item : Namespace_Type;
    Found : Boolean;
  begin
    Index := String_Mng.Locate (Name.Image, ":");
    if Index <= 1 then
      -- Not qualified or ":<suffix>" => Default namespace
      if Element then
        -- Default namespace for elements
        Item.Prefix.Set_Null;
      else
        -- No default for attributes
        Namespace.Set_Null;
        return;
      end if;
    else
      -- Extract prefix
      Item.Prefix := Name.Uslice (1, Index - 1);
    end if;
    -- Read namespace
    Ctx.Namespace_List.Search (Item, Found);
    if Found then
      Ctx.Namespace_List.Read (Item);
      Namespace := Item.Namespace;
    else
      Namespace.Set_Null;
    end if;
  end Get;

end Namespaces;

