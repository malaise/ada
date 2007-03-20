with Unique_List;
separate (Xml_Parser)
-- Entity management
package body Entity_Mng is

  -- The stored entities
  type Entity_Type is record
    Name : Asu_Us;
    Value : Asu_Us;
  end record;
  type Entity_Access is access all Entity_Type;
  procedure Set (To : out Entity_Type; Val : in Entity_Type) is
  begin
    To := Val;
  end Set;
  function Image (Entity : Entity_Type) return String is
  begin
    return Asu.To_String (Entity.Name);
  end Image;
  function "=" (Current : Entity_Type; Criteria : Entity_Type) return Boolean is
    use type Asu_Us;
  begin
    return Current.Name = Criteria.Name;
  end "=";
  package Entity_List_Mng is new Unique_List (Entity_Type, Entity_Access,
             Set, Image, "=");
  Entity_List : Entity_List_Mng.List_Type;

  -- Initialise with default entities
  procedure Initialise is
  begin
    -- Reset all entities
    Entity_List_Mng.Delete_List (Entity_List);
    -- Load predefined entities
    Add (Asu.To_Unbounded_String ("amp"),   Asu.To_Unbounded_String ("&"));
    Add (Asu.To_Unbounded_String ("lt"),    Asu.To_Unbounded_String ("<"));
    Add (Asu.To_Unbounded_String ("gt"),    Asu.To_Unbounded_String (">"));
    Add (Asu.To_Unbounded_String ("quot"),  Asu.To_Unbounded_String (""""));
    Add (Asu.To_Unbounded_String ("aquot"), Asu.To_Unbounded_String ("'"));
  end Initialise;

  -- Store an entity
  procedure Add (Name, Value : in Asu_Us) is
    Entity : Entity_Type;
  begin
    Entity := (Name, Value);
    Entity_List_Mng.Insert (Entity_List, Entity);
    Trace ("Added entity name " & Asu.To_String (Name)
         & " value " & Asu.To_String (Value));
  end Add;

  -- Check if an entity exists
  function Exists (Name : Asu_Us) return Boolean is
    Entity : Entity_Type;
    Found : Boolean;
  begin
    -- Find entity with the given name
    Entity.Name := Name;
    Entity_List_Mng.Search (Entity_List, Entity, Found);
    return Found;
  end Exists;

  -- Get value of an entity. Raises Parse_Error if none
  function Get (Name : Asu_Us) return Asu_Us is
    Entity : Entity_Type;
  begin
    -- Read entity with the given name
    Entity.Name := Name;
    Entity_List_Mng.Read (Entity_List, Entity, Entity);
    Trace ("Read entity name " & Asu.To_String (Entity.Name)
         & " value " & Asu.To_String (Entity.Value));
    return Entity.Value;
  exception
    when Entity_List_Mng.Not_In_List =>
      raise Entity_Not_Found;
  end Get;

end Entity_Mng;

