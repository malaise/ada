with Ada.Strings.Unbounded;
with Basic_Proc, Parser;
package body Tree_Mng is

  -- Ada unbounded strings
  package Asu renames Ada.Strings.Unbounded;
  subtype Asu_Us is Asu.Unbounded_String;
  Asu_Null : constant Asu_Us := Asu.Null_Unbounded_String;
  function Asu_Tus (Str : String) return Asu_Us
                   renames Asu.To_Unbounded_String;
  function Asu_Ts (Str : Asu_Us) return String
                   renames Asu.To_String;
  use type Asu_Us;
  use type Sourcer.Src_Kind_List;

  -- Error
  Error_Raised : exception renames Sourcer.Error_Raised;
  procedure Error (Msg : in String) is
  begin
    Basic_Proc.Put_Line_Error ("INTERNAL ERROR: " & Msg & ".");
    raise Error_Raised;
  end Error;

  -- List of direct With from first Origin to current
  Rope : Sourcer.Src_List_Mng.List_Type;

  -- For parsing list of With / subunits
  function Is_Sep (C : Character) return Boolean is
  begin
    return C = Sourcer.Separator;
  end Is_Sep;

  -- Build the children (subunits or withed specs)
  procedure Build_Children (List : in Asu_Us;
                            Kind : in Sourcer.Src_Kind_List) is
    Iter : Parser.Iterator;
    Crit : Sourcer.Src_Dscr;
    Found : Boolean;
  begin
    if List = Asu_Null then
      return;
    end if;
    -- Parse list with Parser and insert nodes
    Iter.Set (Asu_Ts (List), Is_Sep'Access);
    loop
      Crit.Kind := Kind;
      Crit.Unit := Asu_Tus (Iter.Next_Word);
      exit when Crit.Unit = Asu_Null;
      -- Look for unit and insert it in tree
      if Kind = Sourcer.Unit_Spec then
        -- Look for spec or standalone body
        Sourcer.List.Search (Crit, Found);
        if Found then
          Sourcer.List.Read (Crit, Crit);
        else
          Crit.Kind := Sourcer.Unit_Body;
          Sourcer.List.Search (Crit, Found);
          -- Found a body, read to raise ERROR if not standalone
          if Found then
            Sourcer.List.Read (Crit, Crit);
            if not Crit.Standalone then
              -- Found a body without spec and not standalone
              raise Sourcer.Src_List_Mng.Not_In_List;
            end if;
          end if;  
        end if;
      else
        Found := True;
        Sourcer.List.Read (Crit, Crit);
      end if;
      if Found then
        -- Record is found and read
        Build (Crit);
      end if;
    end loop;
  end Build_Children;

  -- Build a node
  procedure Build (Origin : in Sourcer.Src_Dscr) is
    Found : Boolean;
    Child : Sourcer.Src_Dscr;
    Kind : Asu_Us;
  begin
    -- Insert ourself
    if Tree.Is_Empty then
      -- First node
      Tree.Insert_Father ((Origin, False));
      Rope.Insert (Origin);
    else
      Rope.Search (Origin, Found);
      if Found then
        -- Current Origin already exists => Looping
        Tree.Insert_Child ((Origin, True));
        Tree.Move_Father;
        return;
      else
        -- Normal insertion of a new node
        Tree.Insert_Child ((Origin, False));
        Rope.Insert (Origin);
      end if;
    end if;
    if Origin.Kind = Sourcer.Unit_Spec then
      -- A spec: Insert body
      Kind := Asu_Tus ("body");
      if not Origin.Standalone then
        Child.Unit := Origin.Unit;
        Child.Kind := Sourcer.Unit_Body;
        Sourcer.List.Read (Child, Child);
        Build (Child);
      end if;
    end if;
    if (Origin.Kind = Sourcer.Unit_Spec
       or else (Origin.Kind = Sourcer.Unit_Body and then Origin.Standalone) )
    and then Sourcer.Has_Dot (Origin.Unit) then
      -- A Child (spec or standalone body): Insert parent spec
      Kind := Asu_Tus ("parent");
      Child.Unit := Origin.Parent;
      Child.Kind := Sourcer.Unit_Spec;
      Sourcer.List.Read (Child, Child);
      Build (Child);
    end if;
    if Origin.Kind = Sourcer.Unit_Body then
      -- A Body: Insert subunits
      Kind := Asu_Tus ("subunit");
      Build_Children (Origin.Subunits, Sourcer.Subunit);
    end if;
    -- Any unit: Insert withed units
    Kind :=  Asu_Tus ("withed");
    Build_Children (Origin.Witheds, Sourcer.Unit_Spec);
    -- Done, move up
    if Tree.Has_Father then
      Tree.Move_Father;
      Rope.Delete (Origin);
    end if;
  exception
    when Sourcer.Src_List_Mng.Not_In_List =>
      Error ("Cannot find " & Asu_Ts (Kind) & " of " & Sourcer.Image (Origin));
  end Build;

  -- Dump one element of the tree
  function Dump_One (Dscr : in Src_Dscr;
                     Level : in Natural) return Boolean is
    Str : Asu_Us;
  begin
    for I in 1 .. Level loop
      Asu.Append (Str, "  ");
    end loop;
    Asu.Append (Str, Sourcer.Image (Dscr.Dscr));
    if Dscr.Looping then
      Asu.Append (Str, " -->X");
    end if;
    Basic_Proc.Put_Line_Output (Asu_Ts (Str));
    return True;
  end Dump_One;

  procedure Dump is
  begin
    Tree.Iterate (Dump_One'Access);
  end Dump;

end Tree_Mng;

