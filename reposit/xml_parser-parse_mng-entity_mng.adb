with Ada.Characters.Latin_1;
with Unique_List, Int_Io, Utf_8;
separate (Xml_Parser.Parse_Mng)
-- Entity management
package body Entity_Mng is

  -- Store an entity
  procedure Store (The_Entities : in out Entity_List_Mng.List_Type;
                   Name, Value : in Asu_Us; Parameter : in Boolean;
                   Log : in Boolean) is
    Entity : Entity_Type;
  begin
    Entity := (Parameter, Name, Value);
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
    Exp_Len : Natural;
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
      Exp_Len := 4 + Code'Length - 2;
    else
      -- Skip "#"
      Start := Code'First + 1;
      Int_Io.Get (Code(Start .. Code'Last), Res, Last);
      Exp_Len := Code'Length - 1;
    end if;
    -- Check all characters have been was got
    if Last /= Exp_Len then
      raise Constraint_Error;
    end if;
    -- Check for valid values
    if Res = 16#9# or else Res = 16#A# or else Res = 16#D#
    or else (Res >= 16#20# and then Res <= 16#D7FF#)
    or else (Res >= 16#E0000# and then Res <= 16#FFFD#) then
      return Res;
    else
      raise Constraint_Error;
    end if;
  exception
    when others =>
      Trace ("Invalid char code " & Code);
      raise Constraint_Error;
  end Code_Of;

  -- Initialise with default entities
  procedure Initialise (The_Entities : in out Entity_List_Mng.List_Type) is
  begin
    -- Reset all entities
    Entity_List_Mng.Delete_List (The_Entities);
    -- Load predefined entities
    Store (The_Entities, Asu_Tus ("amp"),   Asu_Tus ("&"), False, False);
    Store (The_Entities, Asu_Tus ("lt"),    Asu_Tus ("<"), False, False);
    Store (The_Entities, Asu_Tus ("gt"),    Asu_Tus (">"), False, False);
    Store (The_Entities, Asu_Tus ("quot"),  Asu_Tus (""""), False, False);
    Store (The_Entities, Asu_Tus ("aquot"), Asu_Tus ("'"), False, False);
  end Initialise;

  -- Store an entity
  procedure Add (The_Entities : in out Entity_List_Mng.List_Type;
                 Name, Value : in Asu_Us; Parameter : in Boolean) is
    use type Asu_Us;
  begin
    if Name = Asu_Null then
      Trace ("Storing an empty entity name");
      raise Internal_Error;
    end if;
    Store (The_Entities, Name, Value, Parameter, True);
  end Add;

  -- Check if an entity exists
  procedure Exists (The_Entities : in out Entity_List_Mng.List_Type;
                    Name : in Asu_Us; Parameter : in Boolean;
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
  procedure Get (The_Entities : in out Entity_List_Mng.List_Type;
                 Name : in Asu_Us; Parameter : in Boolean;
                 Got : out Asu_Us) is
    Code : Natural;
    Entity : Entity_Type;
    use type Asu_Us;
  begin
    -- Resolve Character reference
    if not Parameter and then Name /= Asu_Null
    and then Asu.Element (Name, 1) = '#' then
      Code := Code_Of (Asu_Ts (Name));
      Got := Asu_Tus (Utf_8.Encode (Code));
      return;
    end if;
    -- Read entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Entity_List_Mng.Read (The_Entities, Entity, Entity);
    Trace ("Read entity name " & Asu_Ts (Entity.Name)
         & " value " & Asu_Ts (Entity.Value));
    Got := Entity.Value;
  exception
    when Entity_List_Mng.Not_In_List =>
      Trace ("Unknown entity name " & Asu_Ts (Entity.Name));
      raise Entity_Not_Found;
  end Get;

end Entity_Mng;

