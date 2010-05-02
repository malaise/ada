separate (Xml_Checker)

procedure Canon_Callback (Ctx  : in Xml_Parser.Ctx_Type;
                          Node : in Xml_Parser.Node_Update) is
  Clone : Xml_Parser.Node_Update;
  Str : Asu_Us;
  Len : Natural;
  use type Xml_Parser.Node_Kind_List, Xml_Parser.Attributes_Access,
           Xml_Parser.Stage_List, Asu_Us;
begin
  -- Skip xml and DOCTYPE
  if Node.Stage = Xml_Parser.Prologue then
    if Node.Kind = Xml_Parser.Element and then Asu_Ts (Node.Name) = "xml" then
      return;
    elsif Node.Kind = Xml_Parser.Text then
      return;
    end if;
  end if;

  -- Working copy of the node
  Clone := Node;

  -- Sort attributes
  -- @@@
  -- Replace characters in attribute values and texts
  -- @@@

  -- Empty element: Replace EmptyElmTag by StartTag and EndTag
  if Node.Creation and then Stage = Xml_Parser.Elements
  and then Node.Kind = Xml_Parser.Element and then not Node.Has_Children then
    -- Creation of empty element: Clone becomes mixed with children
    Clone.Has_Children := True;
    Clone.Is_Mixed := True;
    -- Start Tag
    Str := Asu_Tus (Xml_Parser.Generator.Image (Ctx, Clone, Format, Width));
    Out_Flow.Put (Asu_Ts (Str));
    -- End Tag
    Clone.Creation := False;
    Clone.Attributes := null;
  end if;

  -- Use the Image of Xml_Parser.Generator
  Str := Asu_Tus (Xml_Parser.Generator.Image (Ctx, Clone, Format, Width));
  Len := Asu.Length (Str);

  -- Remove Leading Line_Feed before Root
  -- No prologue at all
  if ( (Cb_Status = Init and then Stage = Xml_Parser.Elements)
       -- Or a prologue already parsed
       or else Stage = Xml_Parser.Prologue)
  -- First element (root)
  and then Clone.Stage = Xml_Parser.Elements
  and then Clone.Kind = Xml_Parser.Element
  -- Starting by Lf
  and then Len >= 1
  and then Asu.Element (Str, 1) = Text_Line.Line_Feed_Char then
    -- Leading Line_Feed of root: remove it
    Asu.Delete (Str, 1, 1);
    Len := Len - 1;
  end if;
  Cb_Status := Done;

  -- Ensure last char is '>', not Lf
  -- Remove trailing Lf from root EndTag and from Tail items
  if (Clone.Stage = Xml_Parser.Elements
  and then Clone.Level = 0
  and then Clone.Kind = Xml_Parser.Element
  and then not Clone.Creation)
  or else Clone.Stage = Xml_Parser.Tail then
    if Len >= 1
    and then Asu.Element (Str, Len) = Text_Line.Line_Feed_Char then
      Asu.Delete (Str, Len, Len);
      Len := Len - 1;
    end if;
  end if;
  -- Prepend Lf to Tail items
  if Clone.Stage = Xml_Parser.Tail then
    Str := Text_Line.Line_Feed_Char & Str;
    Len := Len + 1;
  end if;

  -- Final put
  Out_Flow.Put (Asu_Ts (Str));

  -- Update for next call
  Stage := Clone.Stage;
  -- Ensure that Node attributes are not deallocated twice
  Clone.Attributes := null;
end Canon_Callback;

