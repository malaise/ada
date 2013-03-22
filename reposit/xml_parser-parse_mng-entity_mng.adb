with Gets, Utf_8;
separate (Xml_Parser.Parse_Mng)
-- Entity management
package body Entity_Mng is

  -- Store an entity
  procedure Store (The_Entities : in out Entity_List_Mng.Unique_List_Type;
                   Name, Value : in As.U.Asu_Us;
                   Parameter : in Boolean;
                   Internal : in Boolean;
                   Intern_Dtd : in Boolean;
                   Parsed : in Boolean;
                   Log : in Boolean) is
    Found : Boolean;
    Entity : Entity_Type;
  begin
    Entity := (Name, Value, Parameter, Internal, Intern_Dtd, Parsed);
    -- Re definition of an existing entity is skipped
    Entity_List_Mng.Search (The_Entities, Entity, Found);
    if Found then
      if Log then
        Trace ((if Parameter then "Parameter entity " else "Entity ")
               & Name.Image & " redefined");
      end if;
      return;
    end if;
    Entity_List_Mng.Insert (The_Entities, Entity);
    if Log then
      Trace ("Stored entity name " & Image (Entity)
           & " value " & Value.Image);
    end if;
  end Store;

  -- Code is a string (including the '#'), either "xijkl" for hexa or 'ijklm'
  -- Return the associated Unicode number
  function Code_Of (Code : String) return Natural is
    Res : Natural;
    Start : Natural;
  begin
    -- Get Hexa or decimal value
    if Code (Code'First + 1) = 'x' then
      if Code'Length <= 2 then
        raise Constraint_Error;
      end if;
      -- Skip "#x"
      Start := Code'First + 2;
      Res := Gets.Get_Int ("16#" & Code(Start .. Code'Last) & "#");
    else
      -- Skip "#"
      Start := Code'First + 1;
      Res := Gets.Get_Int (Code(Start .. Code'Last));
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
  procedure Store_Predefined (
          The_Entities : in out Entity_List_Mng.Unique_List_Type;
          Name, Value : in As.U.Asu_Us) is
  begin
    Store (The_Entities, Name, Value, False, True, True, True, False);
  end Store_Predefined;

  -- Initialise with default entities
  procedure Initialise (The_Entities : in out Entity_List_Mng.Unique_List_Type)
                       is
  begin
    -- Reset all entities
    Entity_List_Mng.Delete_List (The_Entities);
    -- Load predefined entities
    Store_Predefined (The_Entities, As.U.Tus ("amp"),  As.U.Tus ("&#38;"));
    Store_Predefined (The_Entities, As.U.Tus ("lt"),   As.U.Tus ("&#60;"));
    Store_Predefined (The_Entities, As.U.Tus ("gt"),   As.U.Tus (">"));
    Store_Predefined (The_Entities, As.U.Tus ("quot"), As.U.Tus (""""));
    Store_Predefined (The_Entities, As.U.Tus ("apos"), As.U.Tus ("'"));
  end Initialise;

  -- Store an entity
  procedure Add (The_Entities : in out Entity_List_Mng.Unique_List_Type;
                 Name, Value : in As.U.Asu_Us;
                 Parameter : in Boolean;
                 Internal : in Boolean;
                 Intern_Dtd : in Boolean;
                 Parsed : in Boolean) is
  begin
    if Name.Is_Null then
      Trace ("Storing an empty entity name");
      raise Internal_Error;
    end if;
    Store (The_Entities, Name, Value, Parameter, Internal, Intern_Dtd, Parsed,
           True);
  end Add;

  -- Check if an entity exists
  function Exists (The_Entities : in out Entity_List_Mng.Unique_List_Type;
                   Name      : in As.U.Asu_Us;
                   Parameter : in Boolean) return Boolean is
    Code : Natural;
    pragma Unreferenced (Code);
    Entity : Entity_Type;
    Found : Boolean;
  begin
    -- Resolve Character reference
    if not Parameter and then not Name.Is_Null
    and then Name.Element (1) = '#' then
      -- To check validity
      Code := Code_Of (Name.Image);
      return True;
    end if;

    -- Find (parameter) entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Entity_List_Mng.Search (The_Entities, Entity, Found);
    return Found;
  end Exists;

  -- Get value of an entity. Raises Parse_Error if none
  function Get (Ctx : in out Ctx_Type;
                Dtd : in out Dtd_Type;
                Context   : in Context_List;
                Name      : in As.U.Asu_Us;
                Parameter : in Boolean) return As.U.Asu_Us is
    Code : Natural;
    Entity : Entity_Type;
    Result : As.U.Asu_Us;
    use type As.U.Asu_Us;
  begin
    -- Resolve Character reference
    if not Parameter and then not Name.Is_Null
    and then Name.Element (1) = '#' then
      if Context = Ref_Dtd then
        Trace ("Forbidden character entity reference " & Name.Image
             & " in dtd");
        raise Entity_Forbidden;
      end if;
      Code := Code_Of (Name.Image);
      return As.U.Tus (Utf_8.Encode (Code));
    end if;

    -- Read entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Dtd.Entity_List.Read (Entity);
    if Parameter then
      Trace ("Read parameter entity name " & Entity.Name.Image
           & " value " & Entity.Value.Image);
    else
      Trace ("Read entity name " & Entity.Name.Image
           & " value " & Entity.Value.Image);
    end if;
    Result := Entity.Value;

    -- Check validity of entity reference v.s. context. May
    --  - Include (leave Got = Value)
    --  - Forbid (raise Entity_Forbidden)
    --  - Bypass (replace Got by "&name;")
    --  - Include as PE (Got = ' ' & Value & ' ')
    case Context is
      when Ref_Xml | Ref_Attribute =>
        if Parameter then
          Trace ("Forbidden parameter entity reference " & Name.Image
               & " in xml");
          raise Entity_Forbidden;
        end if;
        if Context = Ref_Attribute and then not Entity.Internal then
          Trace ("Forbidden external entity reference " & Name.Image
               & " in attribute value");
          raise Entity_Forbidden;
        end if;
        if not Entity.Parsed then
          Trace ("Forbidden unparsed entity reference " & Name.Image
               & " in xml");
          raise Entity_Forbidden;
        end if;
        if Ctx.Standalone and then not Entity.Intern_Dtd then
          -- Reference in standalone document to entity defined
          --  in external marckup
          raise Entity_Standalone;
        end if;
      when Ref_Entity =>
        if not Parameter then
          if Entity.Parsed then
            -- Bypass => return the "&name;"
            Trace ("Bypassing entity " & Name.Image);
            Result := "&" & Entity.Name & ";";
          else
            Trace ("Unexpected unparsed entity reference " & Name.Image
                 & " in entity value");
            raise Entity_Forbidden;
          end if;
        end if;
      when Ref_Dtd | Ref_Dtd_Mark | Ref_Dtd_Content =>
        if not Parameter then
          Trace ("Forbidden non parameter entity reference " & Name.Image
              & " in dtd");
          raise Entity_Forbidden;
        end if;
    end case;

    -- No parameter entity reference with internal Dtd subset markups
    if Parameter
    and then Ctx.Flow.Curr_Flow.Kind = Int_Dtd_Flow
    and then (Context = Ref_Dtd_Mark or else Context = Ref_Dtd_Content
              or else Context = Ref_Entity) then
      Trace ("Forbidden parameter entity reference " & Name.Image
           & " within markup in internal dtd subset");
      raise Entity_Forbidden;
    end if;

    -- Expand the content of external parsed entity
    if Entity.Parsed and then not Entity.Internal then
      Expand_External_Entity (Ctx, Dtd, Name, Result, Result);
    end if;

    if Context = Ref_Dtd then
      -- Include as PE => return ' ' & Value & ' '
      Result := Util.Space & Result & Util.Space;
    end if;
    return Result;

  exception
    when Entity_List_Mng.Not_In_List =>
      Trace ("Unknown entity name " & Entity.Name.Image);
      raise Entity_Not_Found;
  end Get;

end Entity_Mng;

