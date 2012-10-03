-- Parses command line and returns enigma definition
with Ada.Exceptions;
with Argument, Xml_Parser, Environ, Str_Util, Parser, Images;
with Io_Manager;
package body Definition is

  -- Is the definition loaded
  Initialized : Boolean := False;
  -- The loaded definition
  Def : Definition_Rec;
  Start_Byte : Positive := 1;
  Last_Byte : Natural := 0;
  -- The Xml context
  Ctx : Xml_Parser.Ctx_Type;

  -- The configuration file name.
  Default_File_Name : constant String := "enigma.xml";
  File_Var_Name : constant String := "ENIGMA_CONF_FILE";

  -- The keys
  Switches_Key : constant String := "s";
  Rotors_Key : constant String := "r";
  Init_Key : constant String := "i";
  Start_Key : constant String := "f";
  Last_Key : constant String := "l";

  procedure Ple (S : in String) renames Io_Manager.Put_Line_Error;
  procedure Error (Msg : in String) is
  begin
    Ple ("ERROR: " & Msg & ".");
    Io_Manager.New_Line_Error;
    Ple ("Usage: " & Argument.Get_Program_Name
       & " <reflector_def> [ <rotor_defs>  ] [ <rotor_inits> ] [ <switches> ]");
    Ple ("              [ <first_index> ] [ <last_index> ]");
    Ple ("   <reflector_def> ::= <reflector_name>[@<upperletter>]");
    Ple ("   <rotor_defs>    ::= -r{ [#]<rotor_name>@<ring_setting>[#] }");
    Ple ("   <rotor_inits>   ::= -i{ <upperletter> }");
    Ple ("   <switches>      ::= -s{ <upperletter><upperletter> }");
    Ple ("   <first_index>   ::= -f<positive>       (default 1)");
    Ple ("   <last_index>    ::= -l<positive>       (default none)");
    Ple ("   <ring_setting>  ::= <upperletter>");
    Ple ("   <upperletter>   ::= A .. Z");
    Ple ("Up to 4 rotors can be used and one reflector must be defined.");
    Ple ("They must have different names.");
    Ple ("There must be as many rotor offsets as rotors defined.");
    Ple ("Ex, for rotors I (fastest), IV and Beta (slowest): ""Beta@A#IV@Q#I@F"".");
    raise Invalid_Definition;
  end Error;

  ---------------------------------------------------------------------------

  -- Return the configuration file name
  function File_Name return String is
  begin
    if not Environ.Is_Set (File_Var_Name)
    or else Environ.Getenv (File_Var_Name) = "" then
      return Default_File_Name;
    else
      return Environ.Getenv (File_Var_Name);
    end if;
  end File_Name;

  -- Get the Carries definition of a Rotor
  function Get_Carries (Node : Xml_Parser.Element_Type) return String is
    Attrs : Xml_Parser.Attributes_Array (1 ..2);
  begin
    Attrs := Ctx.Get_Attributes (Node);
    if Attrs(1).Name.Image = "Carries" then
      -- Carries is first attribute
      return Attrs(1).Value.Image;
    else
      -- Carries is second attribute
      return Attrs(2).Value.Image;
    end if;
  end Get_Carries;

  -- Get the definition of a rotor or reflector
  function Get_Def (Node : Xml_Parser.Element_Type) return String is
    Child : Xml_Parser.Text_Type;
  begin
    Child := Ctx.Get_Child (Node, 1);
    return Ctx.Get_Text (Child);
  end Get_Def;

  -- Check no dup in string
  function Check_Dup (Str : String) return Boolean is
    Set : Carries_Array := (others => False);
    Id : Types.Lid;
  begin
    -- No need to check lenght because more than 26 Carries would lead to dup
    for I in Str'Range loop
      if Str(I) not in Types.Letter then
        return False;
      end if;
      Id := Types.Id_Of (Str(I));
      if Set(Id) then
        return False;
      end if;
      Set(Id) := True;
    end loop;
    return True;
  end Check_Dup;

  -- Check the semantic of definition of rotors and reflectors
  -- Already checked because DTD:
  --   Unicity of name (cause attr is an ID)
  --   2 attributes for Rotor and 1 for Reflector
  procedure Check_Config (Children : in Xml_Parser.Nodes_Array) is
    Scrambler : Scrambler_Type;
    Reverted : Scrambler_Type;
    use type Types.Lid;
  begin
    for I in Children'Range loop
      if String'(Ctx.Get_Name (Children(I))) = "Rotor" then
        -- Check the Carries has no dup
        if not Check_Dup (Get_Carries (Children(I))) then
          Ple ("ERROR: Invalid Carries definition at line"
              & Natural'Image (Ctx.Get_Line_No (Children(I))));
           raise Invalid_Configuration;
        end if;
      end if;
      begin
        -- Check the text exists and has no dup
        if not Check_Dup (Get_Def (Children(I))) then
          Ple ("ERROR: Duplicated entry in " & Ctx.Get_Name (Children(I)));
          raise Constraint_Error;
        end if;
        -- Set the scrambler, checks length
        Set (Scrambler, Get_Def (Children(I)));
        -- Verify symetry of reflector
        if String'(Ctx.Get_Name (Children(I))) = "Reflector" then
          Reverted := Revert (Scrambler);
          if Reverted /= Scrambler then
            -- Not symetrical
            Ple ("ERROR: Reflector is not symetrical");
            raise Constraint_Error;
          end if;
          -- Verify no identity in reflector
          for I in Scrambler.Mapping'Range loop
            if Scrambler.Mapping(I) = I then
              -- Identity
              Ple ("ERROR: Reflector has identity");
              raise Constraint_Error;
            end if;
          end loop;
        end if;
      exception
        when Invalid_Configuration =>
          raise;
        when others =>
          Ple ("ERROR: Invalid " & Ctx.Get_Name (Children(I))
                & " definition at line"
                & Natural'Image (Ctx.Get_Line_No (Children(I))));
             raise Invalid_Configuration;
      end;
    end loop;
  end Check_Config;

  ---------------------------------------------------------------------------

  -- Set the switches
  procedure Set_Switches (Str : in String) is
    -- "ABCDE....XYZ"
    Init_Str : String (1 .. Types.Nb_Letters);
    Id : Types.Lid;
    use type Types.Lid;
  begin
    -- Init scrambler to default
    for I in Init_Str'Range loop
      Init_Str(I) := Types.Letter_Of (Types.Id_Of (I));
    end loop;
    Set (Def.Switches, Init_Str);
    -- Check string length
    if Str'Length mod 2 /= 0 then
      Error ("Invalid number of switches");
    elsif Str'Length > Types.Nb_Letters then
      Error ("Too many switches (max is "
             & Images.Integer_Image (Types.Nb_Letters)
             & ")");
    end if;
    -- Set switches, check no dup and no identity
    for I in 1 .. Str'Length loop
      if I mod 2 = 1 then
        if Str(I) = Str(I + 1) then
          Error ("Invalid identity in switches");
        end if;
        Id := Types.Id_Of (Str(I));
        if Def.Switches.Mapping(Id) /= Id then
          -- This position is already set in the scrambler
           Error ("Invalid dual definition in switches");
        end if;
        -- Set scrambler value, symetric
        Def.Switches.Mapping(Id) := Types.Id_Of (Str(I + 1));
        Def.Switches.Mapping(Types.Id_Of (Str(I + 1))) := Id;
      end if;
    end loop;
  end Set_Switches;

  -- Get the name of a Rotor
  function Get_Rotor_Name (Node : Xml_Parser.Element_Type) return String is
    Attrs : Xml_Parser.Attributes_Array (1 ..2);
  begin
    Attrs := Ctx.Get_Attributes (Node);
    if Attrs(1).Name.Image = "Name" then
      -- Name is first attribute
      return Attrs(1).Value.Image;
    else
      -- Name is second attribute
      return Attrs(2).Value.Image;
    end if;
  end Get_Rotor_Name;

  -- Get the name of a Reflector
  function Get_Reflector_Name (Node : Xml_Parser.Element_Type) return String is
    Attr : Xml_Parser.Attribute_Rec;
  begin
    Attr := Ctx.Get_Attribute (Node, 1);
    return Attr.Value.Image;
  end Get_Reflector_Name;

  -- Set a rotor
  procedure Set_Rotor (Rotor_Id : in Rotors_Id_Range;
                       Children : in Xml_Parser.Nodes_Array;
                       Name : in String;
                       Offset : in Types.Lid;
                       Init : in Types.Lid) is
    Found : Natural;
  begin
    -- Find the correct child
    Found := 0;
    for I in Children'Range loop
      if String'(Ctx.Get_Name (Children(I))) = "Rotor"
      and then Get_Rotor_Name (Children(I)) = Name then
        Found := I;
        exit;
      end if;
    end loop;
    if Found = 0 then
      Error ("Rotor name " & Name & " not found");
    end if;
    -- Get its characteristics and set the rotor
    declare
      Carries : constant String := Get_Carries (Children(Found));
      Scrambler : constant String := Get_Def (Children(Found));
    begin
      -- Set scrambler
      Set (Def.Rotors(Rotor_Id).Scrambler, Scrambler);
      -- Set carries
      for I in Carries'Range loop
        Def.Rotors(Rotor_Id).Carries(Types.Id_Of (Carries(I))) := True;
      end loop;
    end;
    -- Set Offset and initial position
    Def.Rotors(Rotor_Id).Offset := Offset;
    Def.Rotors(Rotor_Id).Position := Init;
  exception
    when Invalid_Definition =>
      raise;
    when others =>
      Error ("Invalid definition of rotor " & Name);
      raise Invalid_Definition;
  end Set_Rotor;

  -- Delimiter in Rotors string
  function Separing (C : Character) return Boolean is
  begin
    return C = '#';
  end Separing;

  -- Set the rotors
  procedure Set_Rotors (Children : in Xml_Parser.Nodes_Array;
                        Rotor_Str, Initial_Str : in String) is
    Iter : Parser.Iterator;
    Init_Str : String (1 .. Def.Nb_Rotors);
    No_Carry : constant Carries_Array := (others => False);
  begin
    -- If Init_Str is empty => no initial offset
    if Initial_Str = "" then
      Init_Str := (others => 'A');
    else
      Init_Str := Initial_Str;
    end if;
    -- Parse the Rotors definition
    -- <Name>@<OffsetLetter> [ { #<Name>@<OffsetLetter> } ]
    Iter.Set (Rotor_Str, Separing'Access);
    for I in 1 .. Def.Nb_Rotors loop
      declare
        Str : constant String := Iter.Next_Word;
        Arob : Natural;
      begin
        if Str = "" then
          Error ("Missing rotor definition, expecting "
              & Images.Integer_Image (Def.Nb_Rotors));
        end if;
        Arob := Str_Util.Locate (Str, "@");
        if Arob /= Str'Last - 1 then
          Error ("Invalid rotors definition");
        end if;
        -- Check offset letter
        if Str(Str'Last) not in Types.Letter then
          Error ("Invalid rotor ring setting " & Str(Str'Last));
        end if;
        if Init_Str(I) not in Types.Letter then
          Error ("Invalid rotor initial offset setting " & Str(Str'Last));
        end if;
        -- The order is Refl - R3 - R2 - R1 (R1 is turning fastest)
        -- And signal is
        --               +---------------<-
        --               +--------------->-
        -- So first arg is last rotor, last arg is first rotor!
        Set_Rotor (Def.Nb_Rotors - I + 1,
                   Children, Name => Str(Str'First .. Arob - 1),
                   Offset => Types.Id_Of (Str(Str'Last)),
                   Init => Types.Id_Of (Init_Str(I)));
      end;
    end loop;

    -- Only last rotor can have no carry
    for I in 1 .. Def.Nb_Rotors - 1 loop
      if Def.Rotors(I).Carries = No_Carry  then
        Error ("Only last rotor can have no carry");
      end if;
    end loop;

    Iter.Del;
  end Set_Rotors;

  -- Set the reflector
  procedure Set_Reflector (Children : in Xml_Parser.Nodes_Array;
                           Str : in String) is
    Arob : Natural;
    Found : Natural;
  begin
    if Str = "" then
      Error ("Empty reflector definition");
    end if;
    -- Parse and set offset (optional)
    Arob := Str_Util.Locate (Str, "@");
    if Arob = 0 then
      Arob := Str'Last + 1;
      Def.Reflector.Position := 0;
    elsif Arob /= Str'Last - 1 then
      Error ("Invalid reflector definition");
    elsif Str(Str'Last) not in Types.Letter then
      -- Check offset letter
      Error ("Invalid reflector offset setting " & Str(Str'Last));
    else
      Def.Reflector.Position := Types.Id_Of (Str(Str'Last));
    end if;

    -- Find the correct child
    Found := 0;
    for I in Children'Range loop
      if String'(Ctx.Get_Name (Children(I))) = "Reflector"
      and then Get_Reflector_Name (Children(I))
               = Str(Str'First .. Arob  - 1) then
        Found := I;
        exit;
      end if;
    end loop;
    if Found = 0 then
      Error ("Reflector name " & Str(Str'First .. Arob  - 1) & " not found");
    end if;
    -- Get its characteristics and set the reflector
    declare
      Scrambler : constant String := Get_Def (Children(Found));
    begin
      -- Set scrambler
      Set (Def.Reflector.Scrambler, Scrambler);
    end;
  exception
    when Invalid_Definition =>
      raise;
    when others =>
      Error ("Invalid definition of reflector");
      raise Invalid_Definition;
  end Set_Reflector;

  -- Check arguments and load definition
  procedure Set_Definition (Children : in Xml_Parser.Nodes_Array) is
    Rotor_Nb : Rotors_Nb_Range;
  begin
    -- Any key at most once, reflector once and only once
    if Argument.Is_Set (2, Switches_Key) then
      Error ("Too many switches defintions");
    elsif Argument.Is_Set (2, Rotors_Key) then
      Error ("Too many rotors defintions");
    elsif Argument.Is_Set (2, Init_Key) then
      Error ("Too many rotors initial offsets definitions");
    elsif Argument.Is_Set (2, Start_Key) then
      Error ("Too many first index definitions");
    elsif Argument.Is_Set (2, Last_Key) then
      Error ("Too many last index definitions");
    elsif not Argument.Is_Set (1, Argument.Not_Key) then
      Error ("Missing reflector definition");
    elsif Argument.Is_Set (2, Argument.Not_Key) then
      Error ("Too many reflector definitions");
    end if;

    -- Check offset versus rotors, count rotors
    Rotor_Nb := 0;
    if not Argument.Is_Set (1, Rotors_Key) then
      if Argument.Is_Set (Param_Key => Init_Key) then
        Error ("Unexpected rotor initial offsets key");
      end if;
    else
      declare
        Str : constant String := Argument.Get_Parameter (1,Rotors_Key);
      begin
        for I in Str'Range loop
          if Str(I) = '@' then
            Rotor_Nb := Rotor_Nb + 1;
          end if;
        end loop;
      end;
      if Argument.Is_Set (1, Init_Key)
      and then Argument.Get_Parameter (1, Init_Key)'Length /= Rotor_Nb then
        Error ("Invalid number of rotor initial offsets in """
          & Argument.Get_Parameter (1, Init_Key)
          & """, expecting "
          & Images.Integer_Image (Rotor_Nb));
      end if;
    end if;

    -- Initialize result with correct number of rotors
    Def := (Nb_Rotors => Rotor_Nb,
            Switches => Default_Scrambler,
            Rotors => (others => (Default_Scrambler, (others => False), 0, 0)),
            Reflector => (Default_Scrambler, 0));

    -- Set switches
    if Argument.Is_Set (1, Switches_Key) then
      Set_Switches (Argument.Get_Parameter (1, Switches_Key));
    else
      Set_Switches ("");
    end if;

    -- Set rotors
    if Def.Nb_Rotors /= 0 then
      if Argument.Is_Set (1, Init_Key) then
        Set_Rotors (Children, Argument.Get_Parameter (1, Rotors_Key),
                              Argument.Get_Parameter (1, Init_Key));
      else
        Set_Rotors (Children, Argument.Get_Parameter (1, Rotors_Key), "");
      end if;
    end if;

    -- Set reflector
    Set_Reflector (Children, Argument.Get_Parameter (1, Argument.Not_Key));

    -- Set indexes
    if Argument.Is_Set (1, Start_Key) then
      begin
        Start_Byte := Positive'Value (Argument.Get_Parameter (1, Start_Key));
      exception
        when others =>
          Error ("Invalid value for first index");
      end;
    end if;
    if Argument.Is_Set (1, Last_Key) then
      begin
        Last_Byte := Natural'Value (Argument.Get_Parameter (1, Last_Key));
      exception
        when others =>
          Error ("Invalid value for last index");
      end;
    end if;
  exception
    when Invalid_Definition =>
      raise;
    when Err:others =>
      Error ("Internal error " & Ada.Exceptions.Exception_Name (Err)
           & " raised");
  end Set_Definition;

  ---------------------------------------------------------------------------

  -- Load and check the configuration (definition of rotors and reflectors)
  -- Parse arguments and check definition of switches, rotors and reflector
  -- Logs problem on stdout
  procedure Load_Configuration is
    Parse_Ok : Boolean;
    Root : Xml_Parser.Element_Type;
  begin
    if Initialized then
      return;
    end if;
    -- Parse Xml
    begin
      Ctx.Parse (File_Name, Parse_Ok);
      if not Parse_Ok then
        Ple ("ERROR in file " & File_Name & ": " & Ctx.Get_Parse_Error_Message);
        raise Invalid_Configuration;
      end if;
    exception
      when Xml_Parser.File_Error =>
        Ple ("ERROR: File " & File_Name & " not found.");
        raise Invalid_Configuration;
    end;
    Root := Ctx.Get_Root_Element;

    -- Load the children (Rotors and Reflectors) and check
    declare
      Children : constant Xml_Parser.Nodes_Array := Ctx.Get_Children (Root);
    begin
      -- Check definition of Rotors and Reflectors
      Check_Config (Children);
      -- Check the arguments versus the definition and set the definition
      Set_Definition (Children);
      -- Check length of switches
     end;
    Initialized := True;
  exception
    when Invalid_Configuration | Invalid_Definition =>
      raise;
    when others =>
      Ple ("ERROR while parsing file " & File_Name & ".");
      raise Invalid_Configuration;
  end Load_Configuration;

  -- Parse args and fill Def
  procedure Read_Definition (Def : out Definition_Rec) is
  begin
    Load_Configuration;
    Def := Definition.Def;
  end Read_Definition;

  -- Get start byte offset
  function Read_Start_Byte return Positive is
  begin
    Load_Configuration;
    return Start_Byte;
  end Read_Start_Byte;

  -- Get last byte offset (0 if none)
  function Read_Last_Byte return Natural is
  begin
    Load_Configuration;
    return Last_Byte;
  end Read_Last_Byte;

  -- Initialize a scrambler
  procedure Set (Scrambler : out Scrambler_Type; To : String) is
    use type Types.Lid;
  begin
    if To'Length /= Scrambler.Mapping'Length then
      raise Constraint_Error;
    end if;
    for I in To'Range loop
      Scrambler.Mapping(Types.Id_Of (I)) := Types.Id_Of (To(I));
    end loop;
  end Set;

  -- Translate a letter through a scrambler
  function Translate (Scrambler : Scrambler_Type; A_Letter : Types.Lid)
           return Types.Lid is
  begin
    return Scrambler.Mapping(A_Letter);
  end Translate;

  -- Return the revert scrambler
  function Revert (Scrambler : Scrambler_Type) return Scrambler_Type is
    Res : Scrambler_Type;
  begin
    for I in Scrambler.Mapping'Range loop
      Res.Mapping(Scrambler.Mapping(I)) := I;
    end loop;
    return Res;
  end Revert;

end Definition;

