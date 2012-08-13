-- Cannonification of XML
with Ada.Characters.Latin_1;
with Sorts;
separate (Xml_Checker)
procedure Canon_Callback (Ctx  : in Xml_Parser.Ctx_Type;
                          Node : in Xml_Parser.Node_Update) is
  Line_Feed : constant Character := Text_Line.Line_Feed_Char;
  Carriage_Return : constant Character := Ada.Characters.Latin_1.Cr;
  Horiz_Tab : constant Character := Ada.Characters.Latin_1.Ht;

  Clone : Xml_Parser.Node_Update;
  Str : As.U.Asu_Us;
  Len : Natural;
  Col : Natural;
  Done : Boolean;

  -- Sorting attributes:
  -- xmlns namespace has Prefix set to xmlns and Suffix set to the rest
  -- simple attribute has Suffix set to it
  -- attribute with prefix:suffix, but prefix not defined, is split
  -- attribute with prefix:suffix and prefix defined has Uri and Suffix set
  Xmlns : constant As.U.Asu_Us := As.U.Tus ("xmlns");
  type Attr_Rec is record
    -- Index in Node.Attributes
    Index : Positive;
    Uri : As.U.Asu_Us;
    Prefix : As.U.Asu_Us;
    Suffix : As.U.Asu_Us;
  end record;
  type Attr_Array is array (Positive range <>) of Attr_Rec;
  function Less_Than (A, B : Attr_Rec) return Boolean is
    use type As.U.Asu_Us;
  begin
    if not A.Uri.Is_Null and then     B.Uri.Is_Null then return False; end if;
    if     A.Uri.Is_Null and then not B.Uri.Is_Null then return True;  end if;
    if not A.Uri.Is_Null then
      if A.Uri /= B.Uri then return A.Uri < B.Uri;
      else return A.Suffix < B.Suffix;
      end if;
    end if;
    -- Uri is ""
    if A.Prefix  = Xmlns and then B.Prefix /= Xmlns then return True;  end if;
    if A.Prefix /= Xmlns and then B.Prefix  = Xmlns then return False; end if;
    if A.Prefix = Xmlns then return A.Suffix < B.Suffix; end if;
    -- No xmlns
    if A.Prefix /= B.Prefix then return A.Prefix < B.Prefix; end if;
    return A.Suffix < B.Suffix;
  end Less_Than;
  package Attr_Sort is new Sorts (Attr_Rec, Positive, Less_Than, Attr_Array);

  -- Fix text or attribute value
  procedure Fix_Chars (Str : in out As.U.Asu_Us; Is_Text : in Boolean) is
    Len : Natural := Str.Length;
    I : Positive;
    procedure Subchar (S : in String) is
    begin
      Str.Replace (I, I, S);
      Len := Len + S'Length - 1;
    end Subchar;
  begin
    I := 1;
    loop
      exit when I > Len;
      case Str.Element (I) is
        when '&' => Subchar ("&amp;");
        when '<' => Subchar ("&lt;");
        when Carriage_Return => Subchar ("&#xD;");
        when '"' => if not Is_Text then Subchar ("&quot;"); end if;
        when Line_Feed => if not Is_Text then Subchar ("&#xA;"); end if;
        when Horiz_Tab => if not Is_Text then Subchar ("&#x9;"); end if;
        when '>' => if Is_Text then Subchar ("&gt;"); end if;
        when others => null;
      end case;
      I := I + 1;
    end loop;
  end Fix_Chars;

  use type Xml_Parser.Node_Kind_List, Xml_Parser.Attributes_Access,
           Xml_Parser.Stage_List;
begin
  -- Skip xml and DOCTYPE
  if Node.Stage = Xml_Parser.Prologue then
    if Node.Kind = Xml_Parser.Element and then Node.Name.Image = "xml" then
      return;
    elsif Node.Kind = Xml_Parser.Text then
      return;
    end if;
  end if;

  -- Working copy of the node
  Clone := Node;

  -- Sort attributes:
  -- Init table of Attrs
  if Node.Attributes /= null and then Node.Attributes'Length > 1 then
    declare
      -- Local constant copy of original attributes
      Node_Attrs : constant Xml_Parser.Attributes_Array (Node.Attributes'Range)
                 := Node.Attributes.all;
      Attrs : Attr_Array (Node_Attrs'Range);
      use type As.U.Asu_Us;
    begin
      for I in Node_Attrs'Range loop
        Attrs(I).Index := I;
        if Node_Attrs(I).Name = Xmlns then
          -- "xmlns"
          Attrs(I).Prefix := Xmlns;
          Done := True;
        else
          Col := String_Mng.Locate (Node_Attrs(I).Name.Image, ":");
          if Col = 0 then
            -- Pure attribute
            Attrs(I).Suffix := Node_Attrs(I).Name;
            Done := True;
          else
            -- xmlns:suffix
            Attrs(I).Prefix := Node_Attrs(I).Name.Uslice (1, Col - 1);
            Attrs(I).Suffix := Node_Attrs(I).Name.Uslice (
                                  Col + 1, Node_Attrs(I).Name.Length);
            if Attrs(I).Prefix = Xmlns then
              Done := True;
            end if;
          end if;
        end if;
        -- Now see if there is a matching Uri
        if not Done then
          for J in Node_Attrs'Range loop
            if Node_Attrs(J).Name = Xmlns & ":" & Attrs(I).Prefix then
              -- Yes, found it, store Uri, done for this one
               Attrs(I).Uri := Node_Attrs(J).Value;
               exit;
            end if;
          end loop;
        end if;
      end loop;
      -- Now sort the table
      if Attrs'Length < 5 then
        Attr_Sort.Bubble_Sort (Attrs);
      else
        Attr_Sort.Quick_Sort (Attrs);
      end if;
      -- Finally sort the attributes according to the table
      for I in Attrs'Range loop
        Clone.Attributes(I) := Node_Attrs(Attrs(I).Index);
      end loop;
    end;
  end if;

  -- Replace characters in attribute values and texts
  if Clone.Kind = Xml_Parser.Element and then Clone.Attributes /= null then
    for I in Clone.Attributes'Range loop
      Fix_Chars (Clone.Attributes(I).Value, False);
    end loop;
  elsif Clone.Kind = Xml_Parser.Text then
    Fix_Chars (Clone.Name, True);
  end if;

  -- Empty element: Replace EmptyElmTag by StartTag and EndTag
  if Node.Creation and then Stage = Xml_Parser.Elements
  and then Node.Kind = Xml_Parser.Element and then not Node.Has_Children then
    -- Creation of empty element: Clone becomes mixed with children
    Clone.Has_Children := True;
    Clone.Is_Mixed := True;
    -- Start Tag
    Str := As.U.Tus (Xml_Parser.Generator.Image (Ctx, Clone, Format, Width));
    Out_Flow.Put (Str.Image);
    -- End Tag
    Clone.Creation := False;
    Clone.Attributes := null;
  end if;

  -- Use the Image of Xml_Parser.Generator
  Str := As.U.Tus (Xml_Parser.Generator.Image (Ctx, Clone, Format, Width));
  Len := Str.Length;

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
  and then Str.Element (1) = Line_Feed then
    -- Leading Line_Feed of root: remove it
    Str.Delete (1, 1);
    Len := Len - 1;
  end if;
  Cb_Status := Xml_Checker.Done;

  -- Ensure last char is '>', not Lf
  -- Remove trailing Lf from root EndTag and from Tail items
  if (Clone.Stage = Xml_Parser.Elements
  and then Clone.Level = 0
  and then Clone.Kind = Xml_Parser.Element
  and then not Clone.Creation)
  or else Clone.Stage = Xml_Parser.Tail then
    if Len >= 1
    and then Str.Element (Len) = Line_Feed then
      Str.Delete (Len, Len);
      Len := Len - 1;
    end if;
  end if;
  -- Prepend Lf to Tail items
  if Clone.Stage = Xml_Parser.Tail then
    declare
      use type As.U.Asu_Us;
    begin
      Str := Line_Feed & Str;
    end;
    Len := Len + 1;
  end if;

  -- Final put
  Out_Flow.Put (Str.Image);

  -- Update for next call
  Stage := Clone.Stage;
  -- Ensure that Node attributes are not deallocated twice
  pragma Warnings (Off, "useless assignment to ""*"", value never referenced");
  Clone.Attributes := null;
  pragma Warnings (On,  "useless assignment to ""*"", value never referenced");
end Canon_Callback;

