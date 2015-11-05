separate (Dtd_Generator)
procedure Merge (Into : in out Element_Type; Val : in out Element_Type) is
  use type As.U.Asu_Us;

  -- Deviation caused by a given change of Into
  Dev_Skip_Cur_Opt : constant := 0;
  Dev_Skip_Cur_Man : constant := 1;
  Dev_Insert_Val : constant := 2;
  Dev_Step_Both : constant := -1;

  -- False if Val exceeds Max Deviation
  function Check_Deviation (Val : Integer) return Boolean is
  begin
    return Max_Deviation = 0 or else Val <= Max_Deviation;
  end Check_Deviation;

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

  -- The merge of sequences
  package Sequences is
    procedure Merge_Sequences;
  end Sequences;
  package body Sequences is separate;
  procedure Merge_Sequences renames Sequences.Merge_Sequences;

  -- When looking ofr a match
  Index : Natural;
  Found : Boolean;
  -- Count current number of Enum values
  Nb_Enums : Natural;
  -- Current child
  Child : Child_Type;
  -- The attributes
  Val_Attr, Into_Attr : Attr_Type;
  -- Deviation when progressively chaning children as Opt
  Deviation : Natural;

begin
  Dbg ("Merging element " & Val.Name.Image);

  -- Merge children definitions
  -- if same kind then potentially merge lists
  -- else merge kinds
  if Val.Kind = Into.Kind then
    Dbg ("  Same kind " & Mixed_Str (Val.Kind'Img));
    case Val.Kind is
      when Empty | Not_Empty | Any | Pcdata =>
        -- Element remains Empty, Not_Empty | Any or Pcdata
        null;
      when Sequence =>
        -- The most complex case
        Merge_Sequences;
      when Mixed | Choice =>
        Merge_Lists;
    end case;
  else
    -- Handle all heterogeneous combinations
    case Into.Kind is
      when Empty | Not_Empty =>
        case Val.Kind is
          when Sequence =>
            Dbg ("  Empty then Sequence => Copy sequence as optional");
            -- The max number has already been checked when building Val
            -- Check deviation will not exceed max
            -- In Val, no child is Opt
            if Check_Deviation (Val.Children.Length * Dev_Skip_Cur_Man) then
              -- The sequence just got is copied as optional
              Into.Kind := Sequence;
              for I in 1 .. Val.Children.Length loop
                Child := Val.Children.Element (I);
                Child.Opt := True;
                Into.Children.Append (Child);
              end loop;
            else
              Dbg ("    Deviation exceeded => Choice");
              Into.Kind := Choice;
            end if;
          when Mixed =>
            Dbg ("  Empty then Mixed => Mixed");
            Into.Kind := Mixed;
            Into.Children := Val.Children;
          when Pcdata =>
            Dbg ("  Empty then Pcdata => Pcdata");
            Into.Kind := Pcdata;
          when others =>
            null;
        end case;
      when Sequence =>
        case Val.Kind is
          when Empty | Not_Empty =>
            Dbg ("  Sequence then Empty => Change sequence as optional");
            Deviation := 0;
            for I in 1 .. Into.Children.Length loop
              Child := Into.Children.Element (I);
              if not Child.Opt then
                -- Child is changed as Opt
                -- The max number has already been checked when building Val
                -- Check deviation will not exceed max
                if not Check_Deviation (Deviation) then
                  Dbg ("    Deviation exceeded => Choice");
                  Into.Kind := Choice;
                  exit;
                end if;
                Deviation := Deviation + Dev_Skip_Cur_Man;
                Child.Opt := True;
                Into.Children.Replace_Element (I, Child);
              end if;
            end loop;
          when Mixed =>
            Dbg ("  Sequence then Mixed => Mixed merged");
            Into.Kind := Mixed;
            Reduce (Into);
            Merge_Lists;
          when Pcdata =>
            Dbg ("  Sequence then Pcdata => Mixed");
            Into.Kind := Mixed;
            Reduce (Into);
          when others =>
            null;
        end case;
      when Choice =>
        case Val.Kind is
          when Empty | Not_Empty =>
            Dbg ("  Choice then Empty => null");
            null;
          when Sequence =>
            Dbg ("  Choice then Sequence => Choice merged");
            Into.Kind := Choice;
            Reduce (Val);
            Merge_Lists;
          when Mixed =>
            Dbg ("  Choice then Mixed => Mixed merged");
            Into.Kind := Mixed;
            Merge_Lists;
          when Pcdata =>
            Dbg ("  Choice then Pcdata => Mixed");
            Into.Kind := Mixed;
          when others =>
            null;
        end case;
      when Mixed =>
        case Val.Kind is
          when Empty | Not_Empty =>
            Dbg ("  Mixed then Empty => null");
            null;
          when Sequence =>
            Dbg ("  Mixed then Sequence => Mixed merged");
            Reduce (Val);
            Merge_Lists;
          when Pcdata =>
            Dbg ("  Mixed then Pcdata => null");
          when others =>
            null;
        end case;
      when Any =>
        Dbg ("  Any then " & Mixed_Str (Val.Kind'Img) & " => null");
      when Pcdata =>
        case Val.Kind is
          when Empty | Not_Empty =>
            Dbg ("  Pcdata then Empty => null");
          when Sequence =>
            Dbg ("  Pcdata then Sequence => Mixed copy");
            Into.Kind := Mixed;
            Reduce (Val);
            Into.Children := Val.Children;
          when Mixed =>
            Dbg ("  Pcdata then Mixed => Mixed copy");
            Into.Kind := Mixed;
            Into.Children := Val.Children;
          when others =>
            null;
        end case;
    end case;

    -- Something with Val Any => Any
    if Val.Kind = Any then
      Dbg ("  " & Mixed_Str (Into.Kind'Img) & "then Any => Any");
      Into.Kind := Any;
    end if;

    -- Clean children if not needed
    case Into.Kind is
      when Sequence | Choice | Mixed => null;
      when Empty | Not_Empty | Any | Pcdata => Into.Children.Set_Null;
    end case;

    -- End of Same kind or heterogeneous
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

