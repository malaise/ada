with Ada.Characters.Latin_1;
with Unique_List, Int_Io, Utf_8, Utf_16;
separate (Xml_Parser.Parse_Mng)
-- Entity management
package body Entity_Mng is

  -- Store an entity
  procedure Store (The_Entities : in out Entity_List_Mng.List_Type;
                   Name, Value : in Asu_Us;
                   Parameter : in Boolean;
                   Internal : in Boolean;
                   Parsed : in Boolean;
                   Log : in Boolean) is
    Found : Boolean;
    Entity : Entity_Type;
  begin
    Entity := (Name, Value, Parameter, Internal, Parsed);
    -- Re definition of an existing entity is skipped
    Entity_List_Mng.Search (The_Entities, Entity, Found);
    if Found then
      if Log then
        if Parameter then
          Trace ("Parameter entity " & Asu_Ts (Name) & " redefined");
        else
          Trace ("Entity " & Asu_Ts (Name) & " redefined");
        end if;
      end if;
      return;
    end if;
    Entity_List_Mng.Insert (The_Entities, Entity);
    if Log then
      Trace ("Stored entity name " & Image (Entity)
           & " value " & Asu_Ts (Value));
    end if;
  end Store;

  -- Code is a string (including the '#'), either "xijkl" for hexa or 'ijklm'
  -- Return the associated Unicode number
  function Code_Of (Code : String) return Natural is
    Res : Natural;
    Start : Natural;
    Exp_Last : Natural;
    Last : Natural;
  begin
    -- Get Hexa or decimal value
    if Code (Code'First + 1) = 'x' then
      if Code'Length <= 2 then
        raise Constraint_Error;
      end if;
      -- Skip "#x"
      Start := Code'First + 2;
      Int_Io.Get ("16#" & Code(Start .. Code'Last) & "#",
                  Res, Last);
      Exp_Last := 4 + Code'Length - 2;
    else
      -- Skip "#"
      Start := Code'First + 1;
      Int_Io.Get (Code(Start .. Code'Last), Res, Last);
      Exp_Last := Code'Last;
    end if;
    -- Check all characters have been was got
    if Last /= Exp_Last then
      raise Constraint_Error;
    end if;
    -- Check for valid values
    if (Res >= 16#20# and then Res <= 16#D7FF#)
    or else Res = 16#9# or else Res = 16#A# or else Res = 16#D#
    or else (Res >= 16#E000# and then Res <= 16#FFFD#)
    or else (Res >= 16#10000# and then Res <= 16#10FFFF#) then
      return Res;
    else
      raise Constraint_Error;
    end if;
  exception
    when others =>
      Trace ("Invalid char code " & Code);
      raise Invalid_Char_Code;
  end Code_Of;

  -- Store predefined (non parameter, internal, parsed) entity, no log
  procedure Store_Predefined (The_Entities : in out Entity_List_Mng.List_Type;
                              Name, Value : in Asu_Us) is
  begin
    Store (The_Entities, Name, Value, False, True, True, False);
  end Store_Predefined;

  -- Initialise with default entities
  procedure Initialise (The_Entities : in out Entity_List_Mng.List_Type) is
  begin
    -- Reset all entities
    Entity_List_Mng.Delete_List (The_Entities);
    -- Load predefined entities
    Store_Predefined (The_Entities, Asu_Tus ("amp"),  Asu_Tus ("&#38;"));
    Store_Predefined (The_Entities, Asu_Tus ("lt"),   Asu_Tus ("&#60;"));
    Store_Predefined (The_Entities, Asu_Tus ("gt"),   Asu_Tus (">"));
    Store_Predefined (The_Entities, Asu_Tus ("quot"), Asu_Tus (""""));
    Store_Predefined (The_Entities, Asu_Tus ("apos"), Asu_Tus ("'"));
  end Initialise;

  -- Store an entity
  procedure Add (The_Entities : in out Entity_List_Mng.List_Type;
                 Name, Value : in Asu_Us;
                 Parameter : in Boolean;
                 Internal : in Boolean;
                 Parsed : in Boolean) is
    use type Asu_Us;
  begin
    if Name = Asu_Null then
      Trace ("Storing an empty entity name");
      raise Internal_Error;
    end if;
    Store (The_Entities, Name, Value, Parameter, Internal, Parsed, True);
  end Add;

  -- Check if an entity exists
  procedure Exists (The_Entities : in out Entity_List_Mng.List_Type;
                    Name : in Asu_Us;
                    Parameter : in Boolean;
                    Found : out Boolean) is
    Code : Natural;
    pragma Unreferenced (Code);
    Entity : Entity_Type;
    use type Asu_Us;
  begin
    -- Resolve Character reference
    if not Parameter and then Name /= Asu_Null
    and then Asu.Element (Name, 1) = '#' then
      -- To check validity
      Code := Code_Of (Asu_Ts (Name));
      Found := True;
      return;
    end if;

    -- Find (parameter) entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Entity_List_Mng.Search (The_Entities, Entity, Found);
  end Exists;

  -- Get value of an entity. Raises Parse_Error if none
  procedure Get (Ctx : in out Ctx_Type;
                 Dtd : in out Dtd_Type;
                 Context : in Context_List;
                 Name : in Asu_Us;
                 Parameter : in Boolean;
                 Got : out Asu_Us) is
    Code : Natural;
    Entity : Entity_Type;
    use type Asu_Us;
  begin
    -- Resolve Character reference
    if not Parameter and then Name /= Asu_Null
    and then Asu.Element (Name, 1) = '#' then
      if Context = Ref_Dtd then
        Trace ("Forbidden character entity reference " & Asu_Ts (Name)
             & " in dtd");
        raise Entity_Forbidden;
      end if;
      Code := Code_Of (Asu_Ts (Name));
      Got := Asu_Tus (Utf_8.Encode (Code));
      return;
    end if;

    -- Read entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Entity_List_Mng.Read (Dtd.Entity_List, Entity, Entity);
    if Parameter then
      Trace ("Read parameter entity name " & Asu_Ts (Entity.Name)
           & " value " & Asu_Ts (Entity.Value));
    else
      Trace ("Read entity name " & Asu_Ts (Entity.Name)
           & " value " & Asu_Ts (Entity.Value));
    end if;
    Got := Entity.Value;

    -- Check validity of entity reference v.s. context. May
    --  - Include (leave Got = Value)
    --  - Forbid (raise Entity_Forbidden)
    --  - Bypass (replace Got by "&name;")
    --  - Include as PE (Got = ' ' & Value & ' ')
    case Context is
      when Ref_Xml | Ref_Attribute =>
        if Parameter then
          Trace ("Forbidden parameter entity reference " & Asu_Ts (Name)
               & " in xml");
          raise Entity_Forbidden;
        end if;
        if Context = Ref_Attribute and then not Entity.Internal then
          Trace ("Forbidden external entity reference " & Asu_Ts (Name)
               & " in attribute");
          raise Entity_Forbidden;
        end if;
        if not Entity.Parsed then
          Trace ("Forbidden unparsed entity reference " & Asu_Ts (Name)
               & " in xml");
          raise Entity_Forbidden;
        end if;
      when Ref_Entity =>
        if not Parameter then
          if Entity.Parsed then
            -- Bypass => return the "&name;"
            Trace ("Bypassing entity " & Asu_Ts (Name));
            Got := "&" & Entity.Name & ";";
          else
            Trace ("Unexpected unparsed entity reference " & Asu_Ts (Name)
                 & " in entity value");
            raise Entity_Forbidden;
          end if;
        end if;
      when Ref_Dtd | Ref_Dtd_Mark =>
        if not Parameter then
          Trace ("Forbidden non parameter entity reference " & Asu_Ts (Name)
              & " in dtd");
          raise Entity_Forbidden;
        end if;
    end case;

    -- No parameter entity referenc with internal Dtd subset markups
    if Parameter
    and then Ctx.Flow.Curr_Flow.Kind = Int_Dtd_Flow
    and then (Context = Ref_Dtd_Mark or else Context = Ref_Entity) then
      Trace ("Forbidden parameter entity reference " & Asu_Ts (Name)
           & " within markup in internal dtd subset");
      raise Entity_Forbidden;
    end if;

    -- Expand the content of external parsed entity
    if Entity.Parsed and then not Entity.Internal then
      Expand_External_Entity (Ctx, Dtd, Name, Got, Got);
    end if;

    if Context = Ref_Dtd then
      -- Include as PE => return ' ' & Value & ' '
      Got := Util.Space & Got & Util.Space;
    end if;

  exception
    when Entity_List_Mng.Not_In_List =>
      Trace ("Unknown entity name " & Asu_Ts (Entity.Name));
      raise Entity_Not_Found;
  end Get;

end Entity_Mng;

