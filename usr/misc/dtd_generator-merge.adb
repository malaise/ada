separate (Dtd_Generator)
procedure Merge (Into : in out Element_Type; Val : in Element_Type) is
  use type As.U.Asu_Us;

  -- Append to Into the children that are new in Val
  -- Don't care about Opt and Mult cause result will be a Choice or Mixed
  -- Only check that Max_Elements is not reached
  procedure Merge_Lists is
    Child : Child_Type;
    Found : Boolean;
  begin
    Dbg ("  Merging lists");
    for I in 1 .. Val.Children.Length loop
      Child := Val.Children.Element (I);
      Found := False;
      for J in 1 .. Into.Children.Length loop
        if Into.Children.Element (J).Name = Child.Name then
          Dbg ("    Found " & Child.Name.Image);
          Found := True;
          exit;
        end if;
      end loop;
      if not Found then
        -- Check if max of elements is reached
        if Max_Elements /= 0 and then Into.Children.Length = Max_Elements then
          Dbg ("    Max children reached with " & Child.Name.Image & " => Any");
          Into.Kind := Any;
          return;
        else
          -- Append child
          Dbg ("    Append child " & Child.Name.Image);
          Into.Children.Append (Child);
        end if;
      end if;
    end loop;
  end Merge_Lists;

  -- Merge Val sequence into Into
  -- loop // ends when increasing one or both indexes leads to one or both out
  --   if Cur_Into = Cur_Val then
  --     Decrease deviation
  --     Set Mult in Into if Val is Mult and not Into
  --     Increase both indexes
  --     Reset
  --   else
  --     if Cur_Into is Opt then
  --       Go to next Into
  --       Reset
  --     elsif deviation reached then
  --       if Saved then
  --         If max elements reached then give up => Any
  --         Restore
  --         Insert Cur_Val as Opt,
  --         Increase deviation
  --         Increase both indexes
  --       else
  --         Give up => Mixed
  --       end if
  --     else
  --       if not Saved then Save (children of Into, deviation and both indexes)
  --       Set Opt in Into
  --       Increase deviation
  --       Increase Into index
  --     end if
  -- end loop
  -- // Increasing both indexes has increased one if possible
  -- Check deviation and set as Opt all remaining Into
  -- Check max elements and deviation and append as Opt all remaining Val
  procedure Merge_Sequences is
  begin
    -- @@@
    Merge_Lists;
  end Merge_Sequences;


  -- When looking ofr a match
  Index : Natural;
  Found : Boolean;
  -- Count current number of Enum values
  Nb_Enums : Natural;
  -- Current child
  Child : Child_Type;
  -- The attributes
  Val_Attr, Into_Attr : Attr_Type;

begin
  Dbg ("Merging element " & Val.Name.Image);

  -- Merge children definitions
  -- if same kind then potentially merge lists
  -- else merge kinds
  if Val.Kind = Into.Kind then
    Dbg ("  Same kind " & Mixed_Str (Val.Kind'Img));
    case Val.Kind is
      when Empty | Any | Pcdata =>
        -- Element remains Empty, Any or Pcdata
        null;
      when Sequence =>
        -- The most complex case
        Merge_Sequences;
      when Mixed | Choice =>
        Merge_Lists;
    end case;
  else
    -- Handle all heterogenous combinations
    case Into.Kind is
      when Empty =>
        case Val.Kind is
          when Sequence =>
            Dbg ("  Empty then Sequence => Copy sequence as optional");
            -- Check deviation will not exceed max
            if Max_Deviation /= 0
            and then Into.Deviation - Val.Children.Length < -Max_Deviation then
              Dbg ("    Deviation exceeded => Any");
              Into.Kind := Any;
              Into.Children.Set_Null;
            else
              -- The sequence just got is copied as optional
              Into.Kind := Sequence;
              for I in 1 .. Val.Children.Length loop
                Child := Val.Children.Element (I);
                Child.Opt := True;
                Into.Children.Append (Child);
              end loop;
              Into.Deviation := Into.Deviation - Val.Children.Length;
            end if;
          when Mixed =>
            Dbg ("  Empty then Mixed => null");
          when Pcdata =>
            Dbg ("  Empty then Pcdata => null");
          when others =>
            null;
        end case;
      when Sequence =>
        case Val.Kind is
          when Empty =>
            Dbg ("  Sequence then Empty => Change sequence as optional");
            for I in 1 .. Into.Children.Length loop
              Child := Into.Children.Element (I);
              if not Child.Opt then
                -- Child is changed as Opt
                if Max_Deviation /= 0
                and then Into.Deviation = -Max_Deviation then
                  Dbg ("    Deviation exceeded => Any");
                  Into.Kind := Any;
                  Into.Children.Set_Null;
                  exit;
                end if;
                Into.Deviation := Into.Deviation - 1;
                Child.Opt := True;
                Into.Children.Replace_Element (I, Child);
              end if;
            end loop;
          when Mixed =>
            Dbg ("  Sequence then Mixed => Mixed merged");
            Into.Kind := Mixed;
            Merge_Lists;
          when Pcdata =>
            Dbg ("  Sequence then Pcdata => Mixed");
            Into.Kind := Mixed;
          when others =>
            null;
        end case;
      when Choice =>
        case Val.Kind is
          when Empty =>
            Dbg ("  Choice then Empty => null");
            null;
          when Sequence =>
            Dbg ("  Choice then Sequence => Choice merged");
            Into.Kind := Choice;
            Merge_Lists;
          when Mixed =>
            Dbg ("  Choice then Sequence => Mixed merged");
            Into.Kind := Mixed;
            Merge_Lists;
          when Pcdata =>
            Dbg ("  Choice then Sequence => Mixed");
            Into.Kind := Mixed;
          when others =>
            null;
        end case;
      when Mixed =>
        case Val.Kind is
          when Empty =>
            Dbg ("  Mixed then Empty => null");
            null;
          when Sequence =>
            Dbg ("  Mixed then Sequence => Mixed merged");
            Merge_Lists;
          when Pcdata =>
            Dbg ("  Mixed then Empty => null");
          when others =>
            null;
        end case;
      when Any =>
        Dbg ("  Any then " & Mixed_Str (Val.Kind'Img) & " => null");
      when Pcdata =>
        case Val.Kind is
          when Empty =>
            Dbg ("  Pcdata then Empty => null");
          when Sequence =>
            Dbg ("  Pcdata then Sequence => Mixed copy");
            Into.Kind := Mixed;
            Into.Children := Val.Children;
          when Mixed =>
            Dbg ("  Pcdata then Mixed => Mixed copy");
            Into.Kind := Mixed;
            Into.Children := Val.Children;
          when others =>
            null;
        end case;
    end case;
  end if;

  -- Merge new attributes:
  -- if it is new then append,
  -- elsif they are of the same kind then keep this kind
  --   if Enum then append the value if new
  -- else
  --   if one of elements is Cdata then result is Cdata
  --   elsif on is Nmtokens then result is Nmtokens...
  for I in 1 .. Val.Attributes.Length loop
    Val_Attr := Val.Attributes.Element (I);
    Dbg ("  Merging attr " & Val_Attr.Name.Image
         & " " & Mixed_Str (Val_Attr.Kind'Img)
         & (if Val_Attr.Kind = Enum then " Values: " & Val_Attr.Values.Image
            else ""));
    -- Search if it exists in Into attributes
    Index := 0;
    for J in 1 .. Into.Attributes.Length loop
      Into_Attr := Into.Attributes.Element (J);
      if Val_Attr.Name = Into_Attr.Name then
        Dbg ("    Matches previous of kind " & Mixed_Str (Into_Attr.Kind'Img)
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
      Dbg ("    New attr, appended");
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
              Dbg ("    Match enum value " & Value);
              Found := True;
              exit;
            end if;
          end;
        end loop;
        if not Found then
          -- Append new value if not above Max_Enum
          if Max_Enums = 0 or else Nb_Enums < Max_Enums then
            Dbg ("    Append enum value " & Val_Attr.Values.Image);
            Into_Attr.Values.Append (Sep & Val_Attr.Values);
          else
            -- Too many values => NMTOKEN
            Dbg ("    Too many enum values => Nmtoken");
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
          Dbg ("    Lower kind to " & Mixed_Str (Kind'Img));
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
      Dbg ("    Reset previous attr " & Into_Attr.Name.Image & " as Implied");
      Into_Attr.Required := False;
      Into.Attributes.Replace_Element (I, Into_Attr);
    end if;
  end loop;

end Merge;

