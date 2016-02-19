-- Bencoder library that encodes  Xml <-> Bencode
with Trace.Loggers, Long_Longs, Images, Gets, Hexa_Utils, As.U,
     Upper_Str, Mixed_Str, Unbounded_Arrays,
     Xml_Parser.Generator;
package body Bencode is

  -- Unbounded array of bytes
  package Ubytes is new Unbounded_Arrays (Byte, Byte_Array);

  -- Logger of Debug and Error
  Logger_Name : constant String := "Bencode";
  Logger : Trace.Loggers.Logger;

  -- Is Byte from '0' to '9'
  function Is_Digit (B : Positive) return Boolean is
  begin
    return B >= Character'Pos('0') and then B <= Character'Pos('9');
  end Is_Digit;

  -- Decode a Bencoded byte array into a Xml stream
  function Bencode2Xml (Ben_Stream : Byte_Array) return String is
    I : Positive;
    Ctx : Xml_Parser.Generator.Ctx_Type;
    Node : Xml_Parser.Node_Type;

    -- Check that next byte can be read
    procedure Check (Action : in String) is
    begin
      if I = Ben_Stream'Last then
        Logger.Log_Error ("Unexpected end of bencoded array at offset "
                        & Images.Integer_Image (I)
                        & " while " & Action);
        raise Format_Error;
      end if;
    end Check;

    -- Decode an Int
    procedure Decode_Int is
      N : Positive;
      B : Natural;
      Tmp : As.U.Asu_Us;
      New_Node : Xml_Parser.Node_Type;
    begin
      Logger.Log_Debug ("Decoding an Int");
      -- Get [ '-' ] then digits until 'e'
      N := I;
      loop
        Check ("decoding an Int");
        I := I + 1;
        B := Natural (Ben_Stream(I));
        if Is_Digit (B) then
          -- A digit, go on
          Tmp.Append (Character'Val(B));
        elsif B = Character'Pos('e') then
          -- End of the Int
          exit;
        elsif B = Character'Pos('-') and then I = N + 1 then
          -- First char is '-', go on
          Tmp.Append (Character'Val(B));
        else
          Logger.Log_Error ("Unexpected byte " & Character'Val(B)
                           & " at offset " & Images.Integer_Image (I)
                           & " while decoding an Int");
          raise Format_Error;
        end if;
      end loop;
      -- Check validity
      declare
        L : Long_Longs.Ll_Integer;
      begin
        L := Gets.Get_Llint (Tmp.Image);
        -- "-0" is forbidden
        if L = 0 and then Tmp.Element(1) = '-' then
          raise Format_Error;
        end if;
      exception
        when others =>
          Logger.Log_Error ("Invalid Int " & Tmp.Image
                           & " at offset " & Images.Integer_Image (I));
          raise Format_Error;
      end;
      -- Insert element "Int" with text
      Ctx.Add_Child (Node, Int_Name, Xml_Parser.Element, New_Node);
      Ctx.Add_Child (New_Node, Tmp.Image, Xml_Parser.Text, New_Node);
      Logger.Log_Debug ("Got Int " & Tmp.Image);
    end Decode_Int;

    -- Decode an array of bytes
    procedure Decode_Bytes is
      B : Natural;
      Tmp, Str : As.U.Asu_Us;
      Valid : Boolean;
      Len : Natural;
      New_Node : Xml_Parser.Node_Type;
    begin
      Logger.Log_Debug ("Decoding a Byte array");
      -- Get <digits>:
      loop
        B := Natural (Ben_Stream(I));
        exit when B = Character'Pos(':');
        if not Is_Digit (B) then
          Logger.Log_Error ("Invalid char in Byte length " & Character'Val(B)
                           & " at offset " & Images.Integer_Image (I));
          raise Format_Error;
        end if;
        Tmp.Append (Character'Val(B));
        Check ("decoding a Bytes length");
        I := I + 1;
      end loop;
      -- Extract length
      Len := Gets.Get_Int (Tmp.Image);
      Logger.Log_Debug ("Bytes len is" & Len'Img);
      -- Convert the bytes into hexa image
      Tmp.Set_Null;
      Valid := True;
      for J in 1 .. Len loop
        Check ("reading a Bytes array");
        I := I + 1;
        B := Natural (Ben_Stream(I));
        Tmp.Append (Upper_Str (Hexa_Utils.Image (B, 2, '0')));
        Valid := Valid and then B >= Character'Pos(' ')
                       and then B <= Character'Pos('~');
        if Valid then
          Str.Append (Character'Val(B));
        end if;
      end loop;
      -- Insert element "Bytes", either emty or with text
      Ctx.Add_Child (Node, Bytes_Name, Xml_Parser.Element, New_Node);
      -- Insert Str attribute if valid string
      if Valid then
        Ctx.Add_Attribute (New_Node, Str_Name, Str.Image);
      end if;
      if Tmp.Is_Null then
        -- Ensure emty text in (formatted) output
        Ctx.Set_Tag_Empty (New_Node, True);
      else
        -- Add text child
        Ctx.Add_Child (New_Node, Tmp.Image, Xml_Parser.Text, New_Node);
      end if;
      Logger.Log_Debug ("Got Bytes " & Tmp.Image);
      if Valid then
        Logger.Log_Debug ("    Str " & Str.Image);
      end if;
    end Decode_Bytes;

    -- Decode an item, return True except if Allow_End and got 'e'
    function Decode_Item (Allow_End : Boolean) return Boolean;
    procedure Decode_Item;

    -- Decode a list
    procedure Decode_List is
      Res : Boolean;
    begin
      Logger.Log_Debug ("Decoding a List");
      -- Insert element "List" with children
      Ctx.Add_Child (Node, List_Name, Xml_Parser.Element, Node);
      -- Insert items
      while True loop
        Logger.Log_Debug ("  Iterating in List");
        Check ("iterating in List");
        I := I + 1;
        Res := Decode_Item (True);
        exit when not Res;
      end loop;
      -- Back to parent of list
      Node := Ctx.Get_Parent (Node);
      Logger.Log_Debug ("Got List");
    end Decode_List;

    -- Decode a dictionary
    procedure Decode_Dictio is
      Res : Boolean;
    begin
      Logger.Log_Debug ("Decoding a Dictio");
      -- Insert element "Dictio" with children
      Ctx.Add_Child (Node, Dictio_Name, Xml_Parser.Element, Node);
      -- Insert items by pairs
      while True loop
        Logger.Log_Debug ("  Iterating in Dictio");
        Check ("iterating in dictio first");
        I := I + 1;
        Res := Decode_Item (True);
        exit when not Res;
        Check ("iterating in dictio second");
        I := I + 1;
        Decode_Item;
      end loop;
      -- Back to parent of list
      Node := Ctx.Get_Parent (Node);
      Logger.Log_Debug ("Got Dictio");
      null;
    end Decode_Dictio;

    -- Decode an item, return True except if Allow_End and got 'e'
    function Decode_Item (Allow_End : Boolean) return Boolean is
      B : Natural;
    begin
      B := Natural (Ben_Stream(I));
      if B = Character'Pos('i') then
        -- An Int
        Decode_Int;
      elsif Is_Digit (B) then
        -- An array of bytes
        Decode_Bytes;
      elsif B = Character'Pos('l') then
        -- A List
        Decode_List;
      elsif B = Character'Pos('d') then
        -- A dictionary
        Decode_Dictio;
      elsif Allow_End and then B = Character'Pos('e') then
        -- End of list or dictionary
        return False;
      else
        Logger.Log_Error ("Invalid starter " & Character'Val(B)
                         & " at offset " & Images.Integer_Image (I));
        raise Format_Error;
      end if;
      return True;
    end Decode_Item;
    procedure Decode_Item is
      Dummy : Boolean;
    begin
     Dummy := Decode_Item (False);
   end Decode_Item;

  begin
    -- Init logger
    Logger.Init (Logger_Name);
    -- Init Xml tree (set root name)
    Ctx.Clear_Xml;
    Node := Ctx.Get_Root_Element;
    Ctx.Set_Name (Node, Bencode_Name);
    -- Process each item
    Logger.Log_Debug ("Decoding Bencoded array of length"
                    & Natural'Image (Ben_Stream'Length));
    I := Ben_Stream'First;
    while I <= Ben_Stream'Last loop
      -- Decode item
      Decode_Item;
      -- Next item
      I := I + 1;
    end loop;
    -- Return the Xml flow
    return Ctx.Put ( (Kind => Xml_Parser.Generator.Raw, others => <>) );
  end Bencode2Xml;


  -- Encode a Xml string into a Bencoded byte array
  function Xml2Bencode (Xml_Stream : String) return Byte_Array is
    Ctx : Xml_Parser.Ctx_Type;
    Dtd : Xml_Parser.Dtd_Type;
    Ok : Boolean;
    Node : Xml_Parser.Node_Type;
    Result : Ubytes.Unb_Array;

    -- Append a Char or a String to the result
    procedure Append (C : in Character) is
    begin
      Result.Append (Byte (Character'Pos (C)));
    end Append;
    procedure Append (Str : in String) is
    begin
      for I in Str'Range loop
        Append (Str(I));
      end loop;
    end Append;

    procedure Encode_Int is
      Text_Node : Xml_Parser.Node_Type;
      Text : As.U.Asu_Us;
      L : Long_Longs.Ll_Integer;
      use type Xml_Parser.Node_Kind_List;
    begin
      Logger.Log_Debug ("Encoding Int");
      Append ('i');
      -- Node must have one Text child
      if Ctx.Get_Nb_Children (Node) /= 1 then
        Logger.Log_Error ("Int node must have one text child");
        raise Format_Error;
      end if;
      Text_Node := Ctx.Get_Child (Node, 1);
      if Text_Node.Kind /= Xml_Parser.Text then
        Logger.Log_Error ("Int node must have one text child");
        raise Format_Error;
      end if;
      -- Check value
      begin
        Text := Ctx.Get_Text (Text_Node);
        L := Gets.Get_Llint (Text.Image);
        if L = 0 and then Text.Element(1) = '-' then
          -- "-0" is forbidden
          raise Constraint_Error;
        end if;
      exception
        when others =>
          Logger.Log_Error ("Invalid Int value " & Text.Image);
          raise Format_Error;
      end;
      -- Ok
      Append (Text.Image);
      Append ('e');
      Logger.Log_Debug ("Encoded Int " & Text.Image);
    end Encode_Int;

    procedure Encode_Bytes is
      Nb_Attributes : Natural;
      Text_Node : Xml_Parser.Node_Type;
      Text : As.U.Asu_Us;
      Attr : Xml_Parser.Attribute_Rec;
      Bytes : Ubytes.Unb_Array;
      use type Xml_Parser.Node_Kind_List;
    begin
      Logger.Log_Debug ("Encoding Bytes");

      -- Node must have at most one Text child
      if Ctx.Get_Nb_Children (Node) > 1 then
        Logger.Log_Error ("Bytes node must have at most one text child");
        raise Format_Error;
      elsif Ctx.Get_Nb_Children (Node) = 1 then
        Text_Node := Ctx.Get_Child (Node, 1);
        if Text_Node.Kind /= Xml_Parser.Text then
          Logger.Log_Error ("Bytes node child must be text");
          raise Format_Error;
        end if;
      end if;

      -- Node can have one attribute "Str", if yes then use it instead of text
      Nb_Attributes := Ctx.Get_Nb_Attributes (Node);
      if Nb_Attributes > 1 then
        Logger.Log_Error ("Invalid attributes of Bytes");
        raise Format_Error;
      elsif Nb_Attributes = 1 then
        -- Set Bytes from Attribute "Str" if it is set
        -- Check attribute name
        Bytes.Set_Null;
        Attr := Ctx.Get_Attribute (Node, 1);
        if Attr.Name.Image /= Str_Name then
          Logger.Log_Error ("Invalid attribute " & Attr.Name.Image
                          & " of Bytes");
          raise Format_Error;
        end if;
        -- Encode value as Bytes
        Logger.Log_Debug ("Using Bytes attribute " & Attr.Value.Image);
        for I in 1 .. Attr.Value.Length loop
          Bytes.Append (Byte(Character'Pos (Attr.Value.Element (I))));
        end loop;
      else
        -- Node must have one Text child
        if Ctx.Get_Nb_Children (Node) /= 1 then
          Logger.Log_Error ("Bytes node without Str must have one text child");
          raise Format_Error;
        end if;
        -- Check length of text is even (Text node has laready been set)
        Text := Ctx.Get_Text (Text_Node);
        if Text.Length rem 2 /= 0 then
          Logger.Log_Error ("Bytes text has an odd length");
          raise Format_Error;
        end if;
        -- Check and encode Hexa number
        Logger.Log_Debug ("Bytes text is " & Text.Image);
        begin
          for I in 1 .. Text.Length / 2 loop
            -- Extract successive pairs of digits as bytes
            Bytes.Append (Byte(Natural'(
                Hexa_Utils.Value (Text.Slice (I * 2 - 1, I * 2)))));
          end loop;
        exception
          when others =>
            Logger.Log_Error ("Invalid content of Bytes " & Text.Image);
            raise Format_Error;
        end;
      end if;

      -- Done: put <len>:<bytes>
      Append (Images.Integer_Image (Bytes.Length) & ":");
      Result.Append (Bytes);
      Logger.Log_Debug ("Encoded" & Natural'Image (Bytes.Length) & " Bytes");
    end Encode_Bytes;

    -- Encode any item
    procedure Encode_Item;

    procedure Encode_List (Put_Delim : in Boolean) is
    begin
      Logger.Log_Debug ("Encoding List " & Mixed_Str (Put_Delim'Img));
      if Put_Delim then
        Append ('l');
      end if;
      -- Iterate on each child
      declare
        Children : constant Xml_Parser.Nodes_Array
                 := Ctx.Get_Children (Node);
      begin
        for I in Children'Range loop
          Node := Children(I);
          Encode_Item;
        end loop;
      end;
      -- Move back to list node
      if Ctx.Get_Nb_Children (Node) /= 0 then
        Node := Ctx.Get_Parent (Node);
        Logger.Log_Debug ("Up to parent");
      end if;
      if Put_Delim then
        Append ('e');
      end if;
      Logger.Log_Debug ("Encoded List");
    end Encode_List;

    procedure Encode_Dictio is
      Nb_Children : Xml_Parser.Child_Range;
    begin
      Logger.Log_Debug ("Encoding Dictio");
      Append ('d');
      -- Dictio contains pairs of items
      Nb_Children := Ctx.Get_Nb_Children (Node);
      if Nb_Children rem 2 /= 0 then
        Logger.Log_Error ("Dictio " & Ctx.Get_Name (Node)
                        & " has an odd number of children");
        raise Format_Error;
      end if;
      -- Iterate on each child
      declare
        Children : constant Xml_Parser.Nodes_Array
                 := Ctx.Get_Children (Node);
      begin
        for I in Children'Range loop
          Node := Children(I);
          Encode_Item;
        end loop;
      end;
      -- Move back to dictio node
      if Nb_Children /= 0 then
        Node := Ctx.Get_Parent (Node);
        Logger.Log_Debug ("Up to parent");
      end if;
      Append ('e');
      Logger.Log_Debug ("Encoded Dictio");
    end Encode_Dictio;

    procedure Encode_Item is
      Name : As.U.Asu_Us;
    begin
      -- Encode current node according to its Name
      Name := Ctx.Get_Name (Node);
      if Name.Image = Int_Name then
        Encode_Int;
      elsif Name.Image = Bytes_Name then
        Encode_Bytes;
      elsif Name.Image = List_Name then
        Encode_List (True);
      elsif Name.Image = Dictio_Name then
        Encode_Dictio;
      else
        Logger.Log_Error ("Unexpected Xml element name "
                        & Name.Image);
        raise Format_Error;
      end if;
    end Encode_Item;

  begin
    -- Init logger
    Logger.Init (Logger_Name);
    Logger.Log_Debug ("Encoding to Bencode");
    -- Parse the XML stream into a tree
    Ctx.Parse_Prologue (Xml_Stream, Dtd, Ok, Use_Dtd => False);
    if not Ok then
      Logger.Log_Error ("Xml parsing error " & Ctx.Get_Parse_Error_Message);
      raise Format_Error;
    end if;
    Ctx.Parse_Elements (Dtd, Ok);
    if not Ok then
      Logger.Log_Error ("Xml parsing error " & Ctx.Get_Parse_Error_Message);
      raise Format_Error;
    end if;
    -- Encode tree
    Logger.Log_Debug ("Xml flow parsed Ok");
    Node := Ctx.Get_Root_Element;
    if Ctx.Get_Name (Node) /= Bencode_Name then
      Logger.Log_Error ("Xml got element name " & Ctx.Get_Name (Node)
                      & " while expecting " & Bencode_Name);
      raise Format_Error;
    end if;
    Logger.Log_Debug ("Encoding Bencode");
    Encode_List (False);
    -- Dump result
    Logger.Log_Debug ("Encoded a Bencode of length"
                    & Natural'Image (Result.Length));
    return Result.To_Array;
  end Xml2Bencode;

end Bencode;

