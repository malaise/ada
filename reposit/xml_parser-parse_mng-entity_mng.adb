with Ada.Characters.Latin_1;
with Unique_List;
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

  -- If String is "xij" then return it
  -- else it is a natural and return its xij
  function Code_Of (Code : Natural) return String is
    Res : String (1 .. 4);
    function Char_Of (N : in Natural) return Character is
    begin
      if N < 16#0A# then
        return Character'Val (Character'Pos('0') + N);
      else
        return Character'Val (Character'Pos('A') + N - 16#0A#);
      end if;
    end Char_Of;
  begin
    if Code < 16#10# or else Code > 16#FF# then
      raise Constraint_Error;
    end if;
    Res (1) :=  '#';
    Res (2) :=  'x';
    Res (3) := Char_Of (Code / 16);
    Res (4) := Char_Of (Code rem 16);
    return Res;
  end Code_Of;

  -- Initialise with default entities
  procedure Initialise (The_Entities : in out Entity_List_Mng.List_Type) is
    package Acl renames Ada.Characters.Latin_1;
    procedure Add_Char (Code : in Natural) is
    begin
      -- Store as normal entity, no tracing
      Store (The_Entities,
             Asu_Tus (Code_Of (Code)),
             Asu_Tus (Character'Val(Code) & ""), False, False);
    end Add_Char;
  begin
    -- Reset all entities
    Entity_List_Mng.Delete_List (The_Entities);
    -- Load predefined entities
    Store (The_Entities, Asu_Tus ("amp"),   Asu_Tus ("&"), False, False);
    Store (The_Entities, Asu_Tus ("lt"),    Asu_Tus ("<"), False, False);
    Store (The_Entities, Asu_Tus ("gt"),    Asu_Tus (">"), False, False);
    Store (The_Entities, Asu_Tus ("quot"),  Asu_Tus (""""), False, False);
    Store (The_Entities, Asu_Tus ("aquot"), Asu_Tus ("'"), False, False);
    -- HTab, Cr and Lf
    Store (The_Entities, Asu_Tus ("#9"), Asu_Tus (Acl.Ht & ""), False, False);
    Add_Char (16#10#);
    Add_Char (16#13#);
    -- Printable ASCII characters
    for I in 16#20# .. 16#7E# loop
       Add_Char (I);
    end loop;
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

  -- Fix name if its #ijk -> #xlm
  procedure Fix_Name (Name : in out Asu_Us; Parameter : in Boolean) is
    N : Natural;
  begin
    if Parameter
    or else Asu.Length (Name) <= 2
    or else Asu.Element (Name, 1) /= '#'
    or else Asu.Element (Name, 2) = 'x' then
      return;
    end if;
    N := Natural'Value (Asu.Slice (Name, 2, Asu.Length(Name)));
    Name := Asu_Tus (Code_Of (N));
  end Fix_Name;

  -- Check if an entity exists
  procedure Exists (The_Entities : in out Entity_List_Mng.List_Type;
                    Name : in Asu_Us; Parameter : in Boolean;
                    Found : out Boolean) is
    Entity : Entity_Type;
  begin
    -- Find (parameter) entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Fix_Name (Entity.Name, Entity.Parameter);
    Entity_List_Mng.Search (The_Entities, Entity, Found);
  end Exists;

  -- Get value of an entity. Raises Parse_Error if none
  procedure Get (The_Entities : in out Entity_List_Mng.List_Type;
                 Name : in Asu_Us; Parameter : in Boolean;
                 Got : out Asu_Us) is
    Entity : Entity_Type;
  begin
    -- Read entity with the given name
    Entity.Parameter := Parameter;
    Entity.Name := Name;
    Fix_Name (Entity.Name, Entity.Parameter);
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

