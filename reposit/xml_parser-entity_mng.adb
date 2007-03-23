with Ada.Characters.Latin_1;
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

  -- Store an entity
  procedure Store (Name, Value : in Asu_Us; Log : in Boolean) is
    Entity : Entity_Type;
  begin
    Entity := (Name, Value);
    Entity_List_Mng.Insert (Entity_List, Entity);
    if Log then
      Trace ("Stored entity name " & Asu.To_String (Name)
           & " value " & Asu.To_String (Value));
    end if;
  end Store;

  -- Initialise with default entities
  procedure Initialise is
    package Acl renames Ada.Characters.Latin_1;
    function Asu_Tus (Str : String) return Asu_Us
                      renames Asu.To_Unbounded_String;
    procedure Add_Char (Code : in Natural) is
      Str : String (1 .. 3);
      function Char_Of (Code : in Natural) return Character is
      begin
        if Code < 16#0A# then
          return Character'Val (Character'Pos('0') + Code);
        else
          return Character'Val (Character'Pos('A') + Code - 16#0A#);
        end if;
      end Char_Of;
    begin
      Str (1) :=  '#';
      Str (2) := Char_Of (Code / 16);
      Str (3) := Char_Of (Code rem 16);
      if Code < 16#10# or else Code > 16#FF# then
        raise Constraint_Error;
      end if;
      Store (Asu_Tus (Str), Asu_Tus (Character'Val(Code) & ""), False);
    end Add_Char;
  begin
    -- Reset all entities
    Entity_List_Mng.Delete_List (Entity_List);
    -- Load predefined entities
    Store (Asu_Tus ("amp"),   Asu_Tus ("&"), False);
    Store (Asu_Tus ("lt"),    Asu_Tus ("<"), False);
    Store (Asu_Tus ("gt"),    Asu_Tus (">"), False);
    Store (Asu_Tus ("quot"),  Asu_Tus (""""), False);
    Store (Asu_Tus ("aquot"), Asu_Tus ("'"), False);
    -- HTab, Cr and Lf
    Store (Asu_Tus ("#9"), Asu_Tus (Acl.Ht & ""), False);
    Add_Char (16#10#);
    Add_Char (16#13#);
    -- Printable ASCII cahracters
    for I in 16#20# .. 16#7E# loop
       Add_Char (I);
    end loop;
  end Initialise;

  -- Store an entity
  procedure Add (Name, Value : in Asu_Us) is
    Entity : Entity_Type;
  begin
    Store (Name, Value, True);
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

