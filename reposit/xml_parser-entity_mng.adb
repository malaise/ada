with Ada.Characters.Latin_1;
with Unique_List;
separate (Xml_Parser)
-- Entity management
package body Entity_Mng is

  -- The stored entities
  type Entity_Type is record
    Parameter : Boolean;
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
    if Entity.Parameter then
      return "%" & Asu_Ts (Entity.Name);
    else
      return Asu_Ts (Entity.Name);
    end if;
  end Image;
  function "=" (Current : Entity_Type; Criteria : Entity_Type) return Boolean is
    use type Asu_Us;
  begin
    return Current.Parameter = Criteria.Parameter
    and then Current.Name = Criteria.Name;
  end "=";
  package Entity_List_Mng is new Unique_List (Entity_Type, Entity_Access,
             Set, Image, "=");
  Entity_List : Entity_List_Mng.List_Type;

  -- Store an entity
  procedure Store (Name, Value : in Asu_Us; Parameter : in Boolean;
                   Log : in Boolean) is
    Entity : Entity_Type;
  begin
    Entity := (Parameter, Name, Value);
    Entity_List_Mng.Insert (Entity_List, Entity);
    if Log then
      Trace ("Stored entity name " & Image (Entity)
           & " value " & Asu_Ts (Value));
    end if;
  end Store;

  -- Initialise with default entities
  procedure Initialise is
    package Acl renames Ada.Characters.Latin_1;
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
      -- Store as normal entity, no tracing
      Store (Asu_Tus (Str), Asu_Tus (Character'Val(Code) & ""), False, False);
    end Add_Char;
  begin
    -- Reset all entities
    Entity_List_Mng.Delete_List (Entity_List);
    -- Load predefined entities
    Store (Asu_Tus ("amp"),   Asu_Tus ("&"), False, False);
    Store (Asu_Tus ("lt"),    Asu_Tus ("<"), False, False);
    Store (Asu_Tus ("gt"),    Asu_Tus (">"), False, False);
    Store (Asu_Tus ("quot"),  Asu_Tus (""""), False, False);
    Store (Asu_Tus ("aquot"), Asu_Tus ("'"), False, False);
    -- HTab, Cr and Lf
    Store (Asu_Tus ("#9"), Asu_Tus (Acl.Ht & ""), False, False);
    Add_Char (16#10#);
    Add_Char (16#13#);
    -- Printable ASCII cahracters
    for I in 16#20# .. 16#7E# loop
       Add_Char (I);
    end loop;
  end Initialise;

  -- Store an entity
  procedure Add (Name, Value : in Asu_Us; Parameter : in Boolean) is
  begin
    Store (Name, Value, Parameter, True);
  end Add;

  -- Check if an entity exists
  function Exists (Name : Asu_Us; Parameter : Boolean) return Boolean is
    Entity : Entity_Type;
    Found : Boolean;
  begin
    -- Find (parameter) entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Entity_List_Mng.Search (Entity_List, Entity, Found);
    return Found;
  end Exists;

  -- Get value of an entity. Raises Parse_Error if none
  function Get (Name : Asu_Us; Parameter : Boolean) return Asu_Us is
    Entity : Entity_Type;
  begin
    -- Read entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Entity_List_Mng.Read (Entity_List, Entity, Entity);
    Trace ("Read entity name " & Asu_Ts (Entity.Name)
         & " value " & Asu_Ts (Entity.Value));
    return Entity.Value;
  exception
    when Entity_List_Mng.Not_In_List =>
      raise Entity_Not_Found;
  end Get;

end Entity_Mng;

