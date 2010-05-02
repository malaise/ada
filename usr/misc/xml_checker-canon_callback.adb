separate (Xml_Checker)

procedure Canon_Callback (Ctx  : in Xml_Parser.Ctx_Type;
                          Node : in Xml_Parser.Node_Update) is
  Clone : Xml_Parser.Node_Update;
  Str : Asu_Us;
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
    Str := Asu_Tus (Xml_Parser.Generator.Image (Ctx, Clone, Format, Width));
  end if;

  -- Use the Image of Xml_Parser.Generator
  Str := Asu_Tus (Xml_Parser.Generator.Image (Ctx, Node, Format, Width));

  -- Remove Leading Line_Feed before Root
  -- No prologue at all
  if ( (Cb_Status = Init and then Stage = Xml_Parser.Elements)
       -- Or a prologue already parsed
       or else Stage = Xml_Parser.Prologue)
  -- First element (root)
  and then Node.Stage = Xml_Parser.Elements
  and then Node.Kind = Xml_Parser.Element
  -- Starting by Lf
  and then Asu.Length (Str) >= 1
  and then Asu.Element (Str, 1) = Text_Line.Line_Feed_Char then
    -- Leading Line_Feed of root: remove it
    Asu.Delete (Str, 1, 1);
  end if;
  Cb_Status := Done;

  -- Final put
  Out_Flow.Put (Asu_Ts (Str));

  -- Update for next call
  Stage := Node.Stage;
  -- Ensure that Node attributes are not deallocated twice
  Clone.Attributes := null;
end Canon_Callback;

