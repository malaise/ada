separate (Dtd_Generator)
procedure Merge (Into : in out Element_Type; Val : in Element_Type) is
  use type As.U.Asu_Us;

  -- The current deviation (insertion of new children as Opt, change of
  --  current children into Opt) before falling back into a choice
  Deviation : Natural;

  -- False if max deviation would be exceeded by adding offset
  function Check_Deviation (Offset : Integer := 1) return Boolean is
  begin
    return Max_Deviation = 0 or else Deviation + Offset <= Max_Deviation;
  end Check_Deviation;

  -- False if max elements would be exceeded by adding offset
  function Check_Elements (Number : in Natural;
                           Offset : Integer := 1) return Boolean is
  begin
    return Max_Elements = 0 or else Number + Offset <= Max_Elements;
  end Check_Elements;

  -- Append to Into the children that are new in Val
  -- Don't care about Opt and Mult cause result will be a Choice or Mixed
  -- Only check that Max_Elements is not reached
  procedure Merge_Lists is
    Child : Child_Type;
    Found : Boolean;
  begin
    Dbg ("  Merging lists into " & Into.Name.Image);
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
        if not Check_Elements (Into.Children.Length) then
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

  -- Reduce Into Choice or Mixed so that each child appears only once
  procedure Reduce is
    Child : Child_Type;
  begin
    Dbg ("  Reducing list of " & Into.Name.Image);
    for I in 1 .. Into.Children.Length - 1 loop
      -- We are deleting children within this loop
      exit when I >= Into.Children.Length;
      Child := Into.Children.Element (I);
      -- Reverse so that deleting J does not affect algo
      for J in reverse I + 1 .. Into.Children.Length loop
        if Into.Children.Element (J).Name = Child.Name then
          Into.Children.Delete (J, J);
        end if;
      end loop;
    end loop;
  end Reduce;

  -- Image of a sequence
  function Sequence_Image (Children : Child_Unbs.Unb_Array) return String is
    function Image (Child : Child_Type) return String is
    begin
      return Child.Name.Image
          & (if Child.Opt then (if Child.Mult then "*" else "?")
             else (if Child.Mult then "+" else ""));
    end Image;
    Res : As.U.Asu_Us;
  begin
    for I in 1 .. Children.Length loop
      if I /= 1 then
        Res.Append (",");
      end if;
      Res.Append (Image (Children.Element (I)));
    end loop;
    return Res.Image;
  end Sequence_Image;

  -- Merge Val sequence into Into
  procedure Merge_Sequences is
    -- Indexes in sequences, 0 when out of bounds
    Vali, Intoi : Natural;
    -- Current Into element
    Curinto : Child_Type;
    -- Current Val element
    Curval : Child_Type;
    -- Current number of successive insertions
    Insertions : Natural := 0;
    -- Saved data: index, deviation and into children
    Saved : Boolean := False;
    Savali, Saintoi : Positive;
    Sadev : Natural;
    Sachild : Child_Unbs.Unb_Array;

    -- Increment Into index, return True if it has overflown
    function Step_Into return Boolean is
    begin
      if Intoi = Into.Children.Length then
        Intoi := 0;
        return True;
      else
        Intoi := Intoi + 1;
        return False;
      end if;
    end Step_Into;

    -- Increment both indexes, return True if one has overflown
    function Step_Both return Boolean is
      Res : Boolean;
    begin
      if Vali = Val.Children.Length then
        Vali := 0;
        Res := True;
      else
        Vali := Vali + 1;
        Res := False;
      end if;
      -- Always also step into
      return Step_Into or else Res;
    end Step_Both;

  begin
    -- Init to start
    Vali := 1;
    Intoi := 1;
    if Logger.Debug_On then
      Dbg ("  Merging sequence " &  Sequence_Image (Val.Children));
      Dbg ("    into " & Sequence_Image (Into.Children));
    end if;

    -- Fusion loop
    loop
      Curinto := Into.Children.Element (Intoi);
      Curval := Val.Children.Element (Vali);
      if Curinto.Name = Val.Children.Element (Vali).Name then
        -- Match: Reset, propagate Mult, step
        Saved := False;
        Insertions := 0;
        Dbg ("    Match for " & Curinto.Name.Image
             & " dev: " & Deviation'Img);
        if Curval.Mult and then not Curinto.Mult then
          Curinto.Mult := True;
          Into.Children.Replace_Element (Intoi, Curinto);
          Dbg ("      Propagate Mult");
        end if;
        if Step_Both then
          Dbg ("      Stepped over");
          exit;
        end if;
      else
        if Curinto.Opt then
          -- Cur Into is optional: skip it
          Dbg ("    Current Into " & Curinto.Name.Image & " is Opt");
          -- Reset and step into
          Saved := False;
          Insertions := 0;
          if Step_Into then
            Dbg ("      Stepped over");
            exit;
          end if;
        elsif (Max_Insertions = 0
               or else Insertions < Max_Insertions)
        and then Check_Deviation then
          -- Cur Into is not optional: Save, insert Val as opt
          -- Check max elements
          if not Check_Elements (Into.Children.Length) then
            Dbg ("    Max children reached with tail. Give up => Any");
            Into.Kind := Any;
            return;
          end if;
          -- Save if needed
          if not Saved then
            Dbg ("      Save");
            Savali := Vali;
            Saintoi := Intoi;
            Sadev := Deviation;
            Sachild := Into.Children;
            Saved := True;
          end if;
          -- Insert
          Curval.Opt := True;
          Into.Children.Insert (Intoi, Curval);
          Deviation := Deviation + 1;
          Insertions := Insertions + 1;
          Dbg ("      Insert Val." & Curval.Name.Image);
          -- Remain on this Into (so incr index) and incr Val
          if Step_Both then
            Dbg ("      Stepped over");
            exit;
          end if;
        else
          if Insertions = 0 and then not Check_Deviation then
            -- Deviation reached when changing Into as Opt.
            Dbg ("    Deviation reached while changing Into as opt."
                 & " Give up => Choice");
            Into.Kind := Choice;
            Merge_Lists;
            return;
          end if;
          -- Insertions or Deviation reached. Need to roll back
          if not Saved then
            -- Nothing saved
            Dbg ("    Insertions or Deviation reached and no saved."
                 & " Give up => Choice");
            Into.Kind := Choice;
            Merge_Lists;
            return;
          end if;
          -- Something saved: Rollback and change current Into as Opt
          -- Restore
          Vali := Savali;
          Intoi := Saintoi;
          Deviation := Sadev;
          Into.Children := Sachild;
          -- Change current Into as Opt
          Curinto := Into.Children.Element (Intoi);
          Curinto.Opt := True;
          Into.Children.Replace_Element (Intoi, Curinto);
          Dbg ("    Insertions or Deviation reached, roll back and change"
               & " Into." & Curinto.Name.Image & " as Opt");
          Saved := False;
          Deviation := Deviation + 1;
          Insertions := 0;
          if Step_Into then
            Dbg ("      Stepped over");
            exit;
          end if;
        end if;
      end if;
    end loop;

    -- Change as Opt tail of Into
    if Intoi /= 0 then
      Dbg ("  Tail of Into");
      loop
        Curinto := Into.Children.Element (Intoi);
        if not Curinto.Opt then
          if not Check_Deviation then
            Dbg ("    Deviation reached. Give up => Choice");
            Into.Kind := Choice;
            Merge_Lists;
            return;
          end if;
          Dbg ("    Change Into." & Curinto.Name.Image & " as Opt");
          Curinto.Opt := True;
          Into.Children.Replace_Element (Intoi, Curinto);
          Deviation := Deviation + 1;
        end if;
        exit when Intoi = Into.Children.Length;
        Intoi := Intoi + 1;
      end loop;
    end if;

    -- Append as Opt the tail of Val
    if Vali /= 0 then
      Dbg ("  Tail of Val");
      if Max_Elements /= 0
      and then Into.Children.Length + Val.Children.Length - Vali + 1
               > Max_Elements then
        Dbg ("    Max children reached with tail. Giving up => Any");
        Into.Kind := Any;
        return;
      end if;
      if not Check_Deviation (Val.Children.Length - Vali + 1) then
        Dbg ("    Deviation reached. Giving up => Choice");
        Into.Kind := Choice;
        Merge_Lists;
        return;
      end if;
      for I in Vali .. Val.Children.Length loop
        Curval := Val.Children.Element (I);
        Dbg ("    Append Val." & Curval.Name.Image & " as Opt");
        Curval.Opt := True;
        Into.Children.Append (Curval);
      end loop;
    end if;

    if Logger.Debug_On then
      Dbg ("  Merged into " & Sequence_Image (Into.Children));
    end if;
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
  Deviation := 0;

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
            -- The max number has already been checked when building Val
            -- Check deviation will not exceed max
            if Check_Deviation (Val.Children.Length) then
              -- The sequence just got is copied as optional
              Into.Kind := Sequence;
              for I in 1 .. Val.Children.Length loop
                Child := Val.Children.Element (I);
                Child.Opt := True;
                Into.Children.Append (Child);
              end loop;
              Deviation := Deviation + Val.Children.Length;
            else
              Dbg ("    Deviation exceeded => Any");
              Into.Kind := Any;
              Into.Children.Set_Null;
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
                -- The max number has already been checked when building Val
                -- Check deviation will not exceed max
                if not Check_Deviation then
                  Dbg ("    Deviation exceeded => Any");
                  Into.Kind := Any;
                  Into.Children.Set_Null;
                  exit;
                end if;
                Deviation := Deviation + 1;
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

    -- Remove duplicates in Choice or Mixed
    if Into.Kind = Choice or else Into.Kind = Mixed then
      Reduce;
    end if;
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
