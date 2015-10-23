separate (Dtd_Generator)
procedure Merge (Into : in out Element_Type; Val : in Element_Type) is
  Index : Natural;
  Found : Boolean;
  Nb_Enums : Natural;
  Val_Attr, Into_Attr : Attr_Type;
  use type As.U.Asu_Us;
begin
  Dbg ("Merging element " & Val.Name.Image & " into " & Into.Name.Image);
  -- Merge new attributes:
  -- If it is new then append,
  -- elsif they are of the same kind then keep this kind
  --  if Enum then append the value if new
  -- Else
  --   If one of elements is Cdata then result is Cdata
  --   Elsif on is Nmtokens then result is Nmtokens...
  for I in 1 .. Val.Attributes.Length loop
    Val_Attr := Val.Attributes.Element (I);
    Dbg ("  Got attr " & Val_Attr.Name.Image
         & " " & Mixed_Str (Val_Attr.Kind'Img)
         & (if Val_Attr.Kind = Enum then " Values: " & Val_Attr.Values.Image
            else ""));
    -- Search if it exists in Into attributes
    Index := 0;
    for J in 1 .. Into.Attributes.Length loop
      Into_Attr := Into.Attributes.Element (J);
      if Val_Attr.Name = Into_Attr.Name then
        Dbg ("    Match attr " & Mixed_Str (Into_Attr.Kind'Img)
             & (if Into_Attr.Kind = Enum then
                 " Values: " & Into_Attr.Values.Image
                else ""));
        Index := J;
        exit;
      end if;
    end loop;
    if Index = 0 then
      -- New attribute => Append as IMPLIED
      Val_Attr.Required := False;
      Into.Attributes.Append (Val_Attr);
      Dbg ("    Appended");
    elsif Val_Attr.Kind = Into_Attr.Kind then
      -- Keep Into attribute definition, append Enum value if needed
      if Val_Attr.Kind = Enum then
        -- Check if Val Values appear in Into Values, also count Into values
        Iter.Reset (Into_Attr.Values.Image);
        Found := False;
        Nb_Enums := 0;
        loop
          declare
            Value : constant String := Iter.Next_Word;
          begin
            exit when Value = "";
            Nb_Enums := Nb_Enums + 1;
            if Value = Val_Attr.Values.Image then
              Dbg ("    Match value " & Value);
              Found := True;
              exit;
            end if;
          end;
        end loop;
        if not Found then
          -- Append new value if not above Max_Enum
          if Max_Enum = 0 or else Nb_Enums < Max_Enum then
            Dbg ("    Append value " & Val_Attr.Values.Image);
            Into_Attr.Values.Append (Sep & Val_Attr.Values);
          else
            -- Too many values => NMTOKEN
            Dbg ("    Too many values => NMTOKEN");
            Into_Attr.Kind := Nmtoken;
            Into_Attr.Values.Set_Null;
          end if;
          Into.Attributes.Replace_Element (Index, Into_Attr);
        end if;
      end if;
    else
      -- Merge on the less stringent kind
      -- At least one is not Enum (otherwise this means both Enums, which has
      --  been already covered)
      for Kind in reverse Attr_Kind_List loop
        if Val_Attr.Kind = Kind or else Into_Attr.Kind = Kind then
          Into_Attr.Kind := Kind;
          Dbg ("    Lower to " & Mixed_Str (Kind'Img));
          Into.Attributes.Replace_Element (Index, Into_Attr);
          exit;
        end if;
      end loop;
    end if;
  end loop;

  -- Adapt to missing attribute: change them as IMPLIED
  for I in 1 .. Into.Attributes.Length loop
    Into_Attr := Into.Attributes.Element (I);
    -- Search if it exists in Into attributes
    Found := False;
    for J in 1 .. Val.Attributes.Length loop
      Val_Attr := Val.Attributes.Element (J);
      if Into_Attr.Name = Val_Attr.Name then
        Found := True;
        exit;
      end if;
    end loop;
    if not Found and then Into_Attr.Required then
      -- This attribute is not used in Val => Ensure it is IMPLIED
      Dbg ("    Reset stored " & Into_Attr.Name.Image & " as IMPLED");
      Into_Attr.Required := False;
      Into.Attributes.Replace_Element (I, Into_Attr);
    end if;
  end loop;

end Merge;

